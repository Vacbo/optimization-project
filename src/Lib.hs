{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib (
    optimizePortfolio,
    optimizePortfolioWithProgress,
    PortfolioResult(..),
    OptimizationProgress(..)
) where

import qualified Data.Vector as V
import Control.Monad (when)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Simulate as S
import qualified DataLoader
import Text.Printf (printf)
import Control.DeepSeq (NFData(..), deepseq)
import Data.IORef
import System.IO (hFlush, stdout)
import Control.Concurrent.Async (mapConcurrently)
import Data.Text (Text)
import qualified WorkStealing as WS
import Data.Time.Clock (getCurrentTime, diffUTCTime)

data PortfolioResult = PortfolioResult {
    weights :: V.Vector Double,
    expectedReturn :: Double,
    risk :: Double,
    sharpeRatio :: Double,
    assetIndices :: [Int] -- Store which assets these weights correspond to
} deriving (Show)

instance NFData PortfolioResult where
    rnf (PortfolioResult w e r s i) = w `deepseq` e `deepseq` r `deepseq` s `deepseq` i `deepseq` ()

data OptimizationProgress = OptimizationProgress {
    completedProgress :: Int, -- Renamed from completedSimulations
    bestSharpeRatio :: Double,
    currentBatch :: Int,      -- Represents current combination number
    totalBatches :: Int,      -- Represents total combinations
    currentMetrics :: Double  -- Sharpe ratio of the current best for this batch/combination
} deriving (Show)

-- | Optimize portfolio with progress tracking, now parallelizing combinations
optimizePortfolioWithProgress :: FilePath
                              -> Int           -- numSimulationsPerCombination
                              -> Int           -- numStocksToSelect (e.g., 25)
                              -> (OptimizationProgress -> IO ())
                              -> IO ([PortfolioResult], V.Vector Text)
optimizePortfolioWithProgress filePath numSimsPerCombo numToSelect progressCallback = do
    putStrLn "[Lib.hs] Loading data..." >> hFlush stdout
    
    -- Start timing data loading
    loadStart <- getCurrentTime
    
    -- DataLoader.loadData returns symbols and matrix
    (loadedSymbols, loadedMatrix_Stocks_x_Dates) <- DataLoader.loadData filePath
    
    loadEnd <- getCurrentTime
    let loadDuration = realToFrac $ diffUTCTime loadEnd loadStart :: Double
    putStrLn $ printf "[Lib.hs] Data loaded in %.2f seconds." loadDuration
    
    putStrLn $ printf "[Lib.hs] Loaded matrix dimensions: %d stocks x %d dates."
        (V.length loadedMatrix_Stocks_x_Dates)
        (if V.length loadedMatrix_Stocks_x_Dates > 0 then V.length (loadedMatrix_Stocks_x_Dates V.! 0) else 0)
    
    -- Timer for covariance calculation
    covStart <- getCurrentTime
    
    let numActualStocksLoaded = V.length loadedMatrix_Stocks_x_Dates
    
    let (workingMatrix_Stocks_x_Dates, n_for_nCk) =
          if numActualStocksLoaded == 0 then
            (V.empty, 0) -- Handle error case
          else if numActualStocksLoaded < numToSelect then
            (loadedMatrix_Stocks_x_Dates, numActualStocksLoaded)  -- Use all stocks if we have fewer than requested
          else
            (loadedMatrix_Stocks_x_Dates, numActualStocksLoaded)  -- Use all loaded stocks
    
    putStrLn $ printf "[Lib.hs] Working with matrix: %d stocks x %d dates."
        (V.length workingMatrix_Stocks_x_Dates)
        (if V.length workingMatrix_Stocks_x_Dates > 0 then V.length (workingMatrix_Stocks_x_Dates V.! 0) else 0)
        
    putStrLn $ printf "[Lib.hs] Working with symbols: %d" (V.length loadedSymbols)
    putStrLn $ printf "[Lib.hs] N for nCk (stocks to choose from): %d, K (stocks to select): %d" n_for_nCk numToSelect

    putStrLn "[Lib.hs] Transposing and calculating full covariance matrix for the working set of stocks..."
    
    -- Calculate the transposed returns and covariance matrix once for the full set
    let workingTransposedMatrix_Dates_x_Stocks = S.transposeMatrix workingMatrix_Stocks_x_Dates
    -- Use the more efficient covariance matrix calculation
    let fullCovMatrix_Stocks_x_Stocks = S.calculateCovarianceMatrixEfficient workingMatrix_Stocks_x_Dates
    
    covEnd <- getCurrentTime
    let covDuration = realToFrac $ diffUTCTime covEnd covStart :: Double
    putStrLn $ printf "[Lib.hs] Full covariance matrix and transposed returns calculated in %.2f seconds." covDuration
    
    -- Combinations timing
    combinationsStart <- getCurrentTime
    
    putStrLn "[Lib.hs] Generating stock combinations (lazily)..."
    putStrLn $ printf "[Lib.hs] About to call nCk with N: %d, k: %d" n_for_nCk numToSelect
    
    -- Convert indices to a Vector for more efficient processing
    let stockIndicesVector = V.fromList [0..(n_for_nCk - 1)]
    
    -- Use the efficient vector-based combinations when possible
    let stockIndexCombinations = 
          if n_for_nCk > 100 
          then map V.toList (V.toList (S.combinationsVector numToSelect stockIndicesVector))
          else S.combinations numToSelect [0..(n_for_nCk - 1)]
          
    let totalCombinations = length stockIndexCombinations
    putStrLn $ printf "[Lib.hs] Mathematically determined total combinations: %d" totalCombinations
    
    combinationsEnd <- getCurrentTime
    let combinationsDuration = realToFrac $ diffUTCTime combinationsEnd combinationsStart :: Double
    putStrLn $ printf "[Lib.hs] Combinations generated in %.2f seconds." combinationsDuration

    putStrLn "[Lib.hs] Starting parallel processing of combinations..."
    
    -- Parallel processing timing
    parallelStart <- getCurrentTime
    
    -- Progress tracking  
    progRef <- newIORef 0
    bestRef <- newIORef 0.0
    let updateProgressAndBest comboNum mbResult = do
            let bestSR = case mbResult of
                  Nothing -> 0.0
                  Just res -> sharpeRatio res
            atomicModifyIORef' bestRef $ \currentBest -> (max currentBest bestSR, ())
            atomicModifyIORef' progRef $ \progress -> (progress + 1, ())
            when (comboNum `mod` 500 == 0 || comboNum == totalCombinations) $ do
                progress <- readIORef progRef
                bestSoFar <- readIORef bestRef
                progressCallback OptimizationProgress {
                    completedProgress = progress,
                    bestSharpeRatio = bestSoFar,
                    currentBatch = comboNum,
                    totalBatches = totalCombinations,
                    currentMetrics = bestSoFar
                }
    
    -- Process each combination of stocks in parallel
    let processCombination :: [Int] -> IO (Maybe PortfolioResult)
        processCombination stockIndices = do
            -- Extract the selected stocks' returns and covariance sub-matrix
            let selectedReturns_Days_x_Stocks = S.selectAssetColumns workingTransposedMatrix_Dates_x_Stocks stockIndices
            let selectedCovMatrix_Stocks_x_Stocks = S.selectSubCovarianceMatrix fullCovMatrix_Stocks_x_Stocks stockIndices
            
            let currentCombo = length stockIndices  -- This is just the count (should be numToSelect)
            
            -- Create dummy stock names for now
            let dummyStockNames = V.replicate numToSelect "STOCK" 
            
            -- Run simulations for this combination
            let maxWeightAttempts = 50 -- Increased limit
            maybeBestResultForCombo <- S.simulatePortfolios numSimsPerCombo maxWeightAttempts dummyStockNames selectedReturns_Days_x_Stocks selectedCovMatrix_Stocks_x_Stocks

            -- Process the best portfolio within this combination
            let portfolioResult = case maybeBestResultForCombo of
                  Nothing -> Nothing  -- No valid portfolio found for this combination
                  Just (ar, av, sr, w) -> 
                      -- Create a PortfolioResult with the asset indices
                      let portfolioRes = PortfolioResult {
                          weights = w,
                          expectedReturn = ar,
                          risk = av,
                          sharpeRatio = sr,
                          assetIndices = stockIndices
                      }
                      in Just portfolioRes
            
            updateProgressAndBest currentCombo portfolioResult
            pure portfolioResult

    putStrLn "[Lib.hs] Using Work Stealing for parallel processing with progress tracking..."
    -- Use the progress-tracking version of work stealing
    maybeBestPortfolios <- WS.processWithWorkStealingProgress 
        (\(processedItems, totalItems) -> do
            let percentValue = if totalItems == 0 then 0.0 else
                               fromIntegral processedItems / fromIntegral totalItems * (100.0 :: Double)
            -- Read the current best sharpe ratio
            currentBest <- readIORef bestRef
            -- Reduce update frequency for progress tracking to every 500 items
            when (processedItems `mod` 500 == 0 || processedItems == totalItems) $ do
                progressCallback OptimizationProgress {
                    completedProgress = processedItems,
                    bestSharpeRatio = currentBest,
                    currentBatch = processedItems,
                    totalBatches = totalCombinations,
                    currentMetrics = currentBest
                })
        processCombination stockIndexCombinations

    parallelEnd <- getCurrentTime
    let parallelDuration = realToFrac $ diffUTCTime parallelEnd parallelStart :: Double
    putStrLn $ printf "[Lib.hs] Parallel processing complete in %.2f seconds. Final sorting..." parallelDuration
    
    -- Filter out Nothings and sort the final results
    let finalResults = catMaybes maybeBestPortfolios
        sortedResults = sortBy (comparing (negate . sharpeRatio)) finalResults
    
    -- Print total execution time
    let totalDuration = realToFrac $ diffUTCTime parallelEnd loadStart :: Double
    putStrLn $ printf "[Lib.hs] Total optimization time: %.2f seconds." totalDuration
    
    return (sortedResults, loadedSymbols)


optimizePortfolio :: FilePath -> Int -> Int -> IO ([PortfolioResult], V.Vector Text)
optimizePortfolio filePath numSimulations numToSelect = do
    optimizePortfolioWithProgress filePath numSimulations numToSelect $ \progress -> do
        let percentValue = if totalBatches progress == 0 then 0.0 else
                           fromIntegral (completedProgress progress) / fromIntegral (totalBatches progress) * (100.0 :: Double)
        printf "\rProcessed Combination %d/%d (%.1f%%), Current Best Overall Sharpe: %.4f " -- Add space at end to overwrite previous longer lines
            (completedProgress progress)
            (totalBatches progress)
            percentValue
            (bestSharpeRatio progress)
        hFlush stdout

-- Removed: generateStratifiedWeights, shuffle, chunksOf
-- Removed: WorkStealing and MemoryEfficient specific code from this file's main logic

