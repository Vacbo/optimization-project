module Main (main) where

import Lib (optimizePortfolioWithProgress, PortfolioResult(..), OptimizationProgress(..))
import System.Environment (getArgs)
import PerformanceMetrics (PerformanceMetrics(..), trackExecutionTime, trackMemoryUsage, collectMetrics, printMetrics)
import Text.Printf (printf)
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime, diffUTCTime)
import System.CPUTime (getCPUTime) -- Import for CPU time

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

            results <- trackExecutionTime "Total Optimization" $
                       trackMemoryUsage "Portfolio Optimization" $
                       optimizePortfolioWithProgress filePath numSimulationsPerCombination numStocksToSelect progressCallback
            
            overallEndTime <- getCurrentTime   -- Wall clock end
            endCPUTime <- getCPUTime         -- CPU time end

            let overallDuration = realToFrac $ diffUTCTime overallEndTime overallStartTime :: Double
            let overallCPUDuration = fromIntegral (endCPUTime - startCPUTime) / 1e12 :: Double -- Pico to seconds

            putStrLn "\n\nOptimization Complete!"
            putStrLn "----------------------------------------"
            putStrLn "Top 5 Portfolios:"
            mapM_ printPortfolio $ take 5 results
            
            -- Print performance metrics
            baseMetrics <- collectMetrics -- Gets GC, Allocations etc.
            
            -- Calculate more accurate efficiency using overall times
            let accurateEfficiency = if overallDuration > 0 then overallCPUDuration / overallDuration else 0.0
            
            -- Override computationTime and parallelizationEfficiency
            let finalMetrics = baseMetrics { 
                                 computationTime = overallDuration, 
                                 parallelizationEfficiency = accurateEfficiency 
                             }
            
            printMetrics finalMetrics
            
        _ -> putStrLn "Usage: optimization-project-exe <file-path> <number-of-simulations>"

-- | Print a portfolio result in a readable format
printPortfolio :: PortfolioResult -> IO ()
printPortfolio res = do -- Use 'res' instead of 'result' to avoid any conflicts
    let result = res :: PortfolioResult -- Explicit type annotation
    putStrLn "----------------------------------------"
    printf "Sharpe Ratio: %.4f\n" (sharpeRatio result)
    printf "Expected Return: %.2f%%\n" (expectedReturn result * 100.0)
    printf "Risk: %.2f%%\n" (risk result * 100.0)
    putStrLn $ "Weights for selected assets (Indices: " ++ show (map (+1) (assetIndices result)) ++ "):"
    -- The weights vector corresponds to the assets in assetIndices
    let ws = V.toList $ weights result
    mapM_ (\(originalIndex, w) -> printf "  Asset %d (Original Index): %.2f%%\n" (originalIndex + 1) (w * 100.0 :: Double)) $ zip (assetIndices result) ws
    -- If you want to show all 30 assets, with 0s for unselected ones:
    -- let numTotalAssets = 30 -- Or derive from data if possible
    -- let fullWeights = V.replicate numTotalAssets 0.0
    -- let finalWeights = V.accum (+) fullWeights $ zip (assetIndices result) (V.toList $ weights result)
    -- mapM_ (\(i, w) -> printf "  Asset %d: %.2f%%\n" (i :: Integer) (w * 100 :: Double)) $ zip [1..] (V.toList finalWeights)
