module Main (main) where

import Lib (optimizePortfolioWithProgress, PortfolioResult(..), OptimizationProgress(..))
import System.Environment (getArgs)
import PerformanceMetrics (PerformanceMetrics(..), trackExecutionTime, trackMemoryUsage, collectMetrics, printMetrics)
import Text.Printf (printf)
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime, diffUTCTime)
import System.CPUTime (getCPUTime) -- Import for CPU time
import Data.Text (Text, unpack) -- Added for unpacking Text symbols
import Data.List (sortBy) -- Added for sorting weights
import Data.Ord (comparing) -- Added for sorting weights

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, numSimsStr] -> do
            let numSimulationsPerCombination = read numSimsStr
                numStocksToSelect = 25 -- As per project requirements
            putStrLn $ "Starting portfolio optimization with " ++ show numSimulationsPerCombination ++ " simulations per stock combination."
            putStrLn $ "Selecting " ++ show numStocksToSelect ++ " stocks for each portfolio."
            putStrLn $ "Using data from: " ++ filePath
            putStrLn "----------------------------------------"
            
            let progressCallback progress = do
                    let prog = progress :: OptimizationProgress 
                        percentComplete = if totalBatches prog == 0 then 0.0 else
                                          fromIntegral (completedProgress prog) / 
                                          fromIntegral (totalBatches prog) * 100.0
                        barWidth = 40
                        filled = round (percentComplete * fromIntegral barWidth / 100.0 :: Double)
                        bar = replicate filled '█' ++ replicate (barWidth - filled) '░'
                    printf "\rProcessed Combo: %d/%d [%s] %.1f%% | Best Overall SR: %.4f"
                        (currentBatch prog) 
                        (totalBatches prog) 
                        bar
                        percentComplete
                        (bestSharpeRatio prog)
                    hFlush stdout

            overallStartTime <- getCurrentTime -- Wall clock start
            startCPUTime <- getCPUTime       -- CPU time start

            -- Execute the optimization and get results and symbols
            -- Explicit type annotation to help GHC
            let optAction :: IO ([PortfolioResult], V.Vector Text)
                optAction = optimizePortfolioWithProgress filePath numSimulationsPerCombination numStocksToSelect progressCallback
            (results, workingSymbols) <- optAction
            
            overallEndTime <- getCurrentTime   -- Wall clock end
            endCPUTime <- getCPUTime         -- CPU time end

            let overallDuration = realToFrac $ diffUTCTime overallEndTime overallStartTime :: Double
            let overallCPUDuration = fromIntegral (endCPUTime - startCPUTime) / 1e12 :: Double -- Pico to seconds

            putStrLn "\n\nOptimization Complete!"
            putStrLn "========================================"
            putStrLn "          Top 5 Portfolios"
            putStrLn "========================================"
            -- Pass workingSymbols and rank to printPortfolio
            mapM_ (\(rank, res) -> printPortfolio workingSymbols rank res) $ zip [1..] (take 5 results) 
            
            -- Print performance metrics
            putStrLn "\n========================================"
            putStrLn "         Performance Metrics"
            putStrLn "========================================"
            baseMetrics <- collectMetrics
            
            let finalMetrics = baseMetrics { 
                                 computationTime = overallDuration, 
                                 parallelizationEfficiency = if overallDuration > 0 then overallCPUDuration / overallDuration else 0.0 
                             }
            printMetrics finalMetrics
            
        _ -> putStrLn "Usage: optimization-project-exe <file-path> <number-of-simulations>"

-- | Print a portfolio result in a more formatted way
printPortfolio :: V.Vector Text -- All possible symbols from the working set
               -> Int             -- Rank of this portfolio (e.g., 1 for the best)
               -> PortfolioResult -- The portfolio data
               -> IO ()
printPortfolio allSymbols rank res = do
    let result = res :: PortfolioResult
    printf "\n--- Portfolio #%d ---\n" rank
    printf "  Sharpe Ratio:    %.4f\n" (sharpeRatio result)
    printf "  Expected Return: %.2f%%\n" (expectedReturn result * 100.0)
    printf "  Risk (StdDev):   %.2f%%\n" (risk result * 100.0)
    
    let selectedAssetIndices = assetIndices result
        portfolioWeights = weights result
        maxIdx = V.length allSymbols - 1
        validIndices = all (\idx -> idx >= 0 && idx <= maxIdx) selectedAssetIndices

    if not validIndices then do
        -- Fallback for safety, shouldn't happen with correct logic
        putStrLn "  Error: Asset indices out of bounds! Using fallback display:"
        putStrLn $ "  Weights (Indices: " ++ show (map (+1) selectedAssetIndices) ++ "):"
        mapM_ (\(originalIndex, w) -> printf "    Asset %d: %.2f%%\n" (originalIndex + 1) (w * 100.0 :: Double)) $ zip selectedAssetIndices (V.toList portfolioWeights)
    else do
        putStrLn "\n  Asset Allocation (Sorted by Weight):"
        -- Create list of (Symbol, Weight)
        let symbolsAndWeights = V.toList $ V.zipWith (\originalIndex weight -> 
                                   (unpack (allSymbols V.! originalIndex), weight) 
                               ) (V.fromList selectedAssetIndices) portfolioWeights
        
        -- Sort by weight descending
        let sortedSymbolsAndWeights = sortBy (flip $ comparing snd) symbolsAndWeights
        
        -- Find max symbol length for alignment
        let maxSymbolLen = maximum $ map (length . fst) sortedSymbolsAndWeights
        
        -- Print aligned weights
        mapM_ (\(symbol, w) -> printf "    %-*s : %6.2f%%\n" maxSymbolLen symbol (w * 100.0 :: Double)) sortedSymbolsAndWeights
