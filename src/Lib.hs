{-# LANGUAGE OverloadedStrings #-}

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
    -- DataLoader.loadData returns symbols and matrix
    (loadedSymbols, loadedMatrix_Stocks_x_Dates) <- DataLoader.loadData filePath
    putStrLn "[Lib.hs] Data loaded." >> hFlush stdout

    let numActualStocksLoaded = V.length loadedMatrix_Stocks_x_Dates
    let numActualDatesLoaded = if V.null loadedMatrix_Stocks_x_Dates then 0 else V.length (loadedMatrix_Stocks_x_Dates V.! 0)

    putStrLn ("[Lib.hs] Loaded matrix dimensions: " ++ show numActualStocksLoaded ++ " stocks x " ++ show numActualDatesLoaded ++ " dates.") >> hFlush stdout

    let targetNumStocksForCombinations = 30 -- As per Dow Jones 30, adjust if needed

    let (workingMatrix_Stocks_x_Dates, n_for_nCk) =
          if numActualStocksLoaded == 0 then
              (V.empty, 0)
          else if numActualStocksLoaded > targetNumStocksForCombinations then
              -- Slice to get the first 'targetNumStocksForCombinations' stocks, keeping all their dates
              (V.slice 0 targetNumStocksForCombinations loadedMatrix_Stocks_x_Dates, targetNumStocksForCombinations)
          else
              -- Use all loaded stocks if fewer than or equal to target
              (loadedMatrix_Stocks_x_Dates, numActualStocksLoaded)

    -- Determine the symbols corresponding to the working matrix
    let workingSymbols =
          if numActualStocksLoaded == 0 then
              V.empty
          else if numActualStocksLoaded > targetNumStocksForCombinations then
              V.slice 0 targetNumStocksForCombinations loadedSymbols
          else
              loadedSymbols

    let numStocksInWorkingSet = V.length workingMatrix_Stocks_x_Dates
    let numDatesInWorkingSet = if V.null workingMatrix_Stocks_x_Dates then 0 else V.length (workingMatrix_Stocks_x_Dates V.! 0)

    putStrLn ("[Lib.hs] Working with matrix: " ++ show numStocksInWorkingSet ++ " stocks x " ++ show numDatesInWorkingSet ++ " dates.") >> hFlush stdout
    putStrLn ("[Lib.hs] Working with symbols: " ++ show (V.length workingSymbols)) >> hFlush stdout
    putStrLn ("[Lib.hs] N for nCk (stocks to choose from): " ++ show n_for_nCk ++ ", K (stocks to select): " ++ show numToSelect) >> hFlush stdout

    when (n_for_nCk == 0 && numToSelect > 0) $
        error "Working stock data is empty, but numToSelect > 0."
    when (n_for_nCk < numToSelect) $
        error $ "Not enough stocks in working data (" ++ show n_for_nCk ++ ") to select desired number (" ++ show numToSelect ++ ")."

    putStrLn "[Lib.hs] Transposing and calculating full covariance matrix for the working set of stocks..." >> hFlush stdout
    -- S.calculateCovarianceMatrix expects (Assets x Days), which is (Stocks x Dates).
    -- So, workingMatrix_Stocks_x_Dates is the correct input format.
    let fullCovMatrix_for_working_set = S.calculateCovarianceMatrix workingMatrix_Stocks_x_Dates

    -- S.transposeMatrix: (Stocks x Dates) -> (Dates x Stocks)
    let fullReturnsMatrix_Days_x_Stocks_for_working_set = S.transposeMatrix workingMatrix_Stocks_x_Dates
    putStrLn "[Lib.hs] Full covariance matrix and transposed returns for working set calculated." >> hFlush stdout

    let assetIndicesAll = [0..n_for_nCk-1] -- Indices for stocks in the working set
    putStrLn "[Lib.hs] Generating stock combinations (lazily)..." >> hFlush stdout
    let stockIndexCombinations = S.combinations numToSelect assetIndicesAll

    putStrLn ("[Lib.hs] About to call nCk with N: " ++ show n_for_nCk ++ ", k: " ++ show numToSelect) >> hFlush stdout
    let totalCombinations = fromIntegral (S.nCk n_for_nCk numToSelect) :: Int
    putStrLn ("[Lib.hs] Mathematically determined total combinations: " ++ show totalCombinations) >> hFlush stdout

    when (totalCombinations == 0 && numToSelect > 0) $
        error $ "No combinations possible for selecting " ++ show numToSelect ++ " from " ++ show n_for_nCk ++ ". Check parameters."

    -- Use an atomic counter for progress reporting
    completedCombosCounter <- newIORef 0
    overallBestSharpeRef <- newIORef (-1/0) -- Negative infinity

    putStrLn "[Lib.hs] Starting parallel processing of combinations..." >> hFlush stdout

    -- Define the action to perform for each combination
    let processCombination :: [Int] -> IO (Maybe PortfolioResult)
        processCombination currentIndexCombination = do
            -- Slice matrices for the current combination
            let selectedReturns_Days_x_Stocks = S.selectAssetColumns fullReturnsMatrix_Days_x_Stocks_for_working_set currentIndexCombination
            let selectedCovMatrix_Stocks_x_Stocks = S.selectSubCovarianceMatrix fullCovMatrix_for_working_set currentIndexCombination
            -- Generate dummy stock names based on selection size, actual names are handled later
            let dummyStockNames = V.replicate (length currentIndexCombination) "DUMMY" 

            -- Run simulations for this combination
            let maxWeightAttempts = 50 -- Increased limit
            maybeBestResultForCombo <- S.simulatePortfolios numSimsPerCombo maxWeightAttempts dummyStockNames selectedReturns_Days_x_Stocks selectedCovMatrix_Stocks_x_Stocks

            -- Process the best portfolio within this combination
            case maybeBestResultForCombo of
                Nothing -> pure Nothing -- No valid portfolio found in simulations for this combo
                Just (bestER, bestRisk, bestSR, bestWeights) -> do
                    let portfolioRes = PortfolioResult {
                                            weights = bestWeights,
                                            expectedReturn = bestER,
                                            risk = bestRisk,
                                            sharpeRatio = bestSR,
                                            assetIndices = currentIndexCombination -- Use the original indices
                                        }
                    -- Update overall best SR atomically
                    atomicModifyIORef' overallBestSharpeRef (\currentBest -> (max currentBest bestSR, ()))

                    -- Increment completed counter and report progress
                    completedCount <- atomicModifyIORef' completedCombosCounter (\c -> (c+1, c+1))
                    currentOverallBestSR <- readIORef overallBestSharpeRef
                    progressCallback OptimizationProgress {
                        completedProgress = completedCount,
                        bestSharpeRatio = if isInfinite currentOverallBestSR && currentOverallBestSR < 0 then 0 else currentOverallBestSR, -- Handle initial -Infinity
                        currentBatch = completedCount, -- Use counter for progress reporting
                        totalBatches = totalCombinations,
                        currentMetrics = bestSR -- SR of the best portfolio found in *this* combination
                    }

                    pure $ Just portfolioRes

    -- Use work stealing instead
    putStrLn "[Lib.hs] Using Work Stealing for parallel processing..." >> hFlush stdout
    maybeBestPortfolios <- WS.processWithWorkStealing processCombination stockIndexCombinations

    -- Filter out Nothings and sort the final results
    let finalResults = catMaybes maybeBestPortfolios
    putStrLn "\n[Lib.hs] Parallel processing complete. Final sorting..." >> hFlush stdout
    -- Return results AND the symbols for the working set
    return (sortBy (flip $ comparing sharpeRatio) finalResults, workingSymbols)


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

