{-# LANGUAGE OverloadedStrings #-}

module PerformanceMetrics (
    PerformanceMetrics(..),
    collectMetrics,
    printMetrics,
    trackExecutionTime,
    trackMemoryUsage
) where

import System.CPUTime
import System.Mem (performGC)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.DeepSeq (NFData, rnf)
import GHC.Stats (getRTSStats, RTSStats(..))
import Data.Word (Word32, Word64)

data PerformanceMetrics = PerformanceMetrics {
    computationTime :: Double,        -- in seconds
    memoryUsage :: Word64,            -- in bytes (changed to Word64)
    parallelizationEfficiency :: Double,  -- percentage
    gcTime :: Double,                -- in seconds
    gcCount :: Word32,               
    totalAllocations :: Word64       -- (changed to Word64)
} deriving (Show)

instance NFData PerformanceMetrics where
    rnf (PerformanceMetrics c m p g gcVal t) = 
        rnf c `seq` rnf m `seq` rnf p `seq` rnf g `seq` rnf gcVal `seq` rnf t

-- | Collect current performance metrics
collectMetrics :: IO PerformanceMetrics
collectMetrics = do
    startTime <- getCurrentTime
    startCPUTime <- getCPUTime
    performGC  -- Force GC to get accurate memory stats
    stats <- getRTSStats
    endTime <- getCurrentTime
    endCPUTime <- getCPUTime
    
    let cpuTime :: Double
        cpuTime = fromIntegral (endCPUTime - startCPUTime) / 1e12  -- Convert to seconds
        realTime :: Double
        realTime = realToFrac $ diffUTCTime endTime startTime
        efficiency :: Double
        efficiency = if realTime > 0 then cpuTime / realTime else 0
        gcTimeUsed = fromIntegral (gc_cpu_ns stats) / 1e9  -- Convert to seconds
    
    return $ PerformanceMetrics {
        computationTime = realTime,
        memoryUsage = allocated_bytes stats - copied_bytes stats, -- This is Word64 - Word64 = Word64
        parallelizationEfficiency = efficiency,
        gcTime = gcTimeUsed,
        gcCount = gcs stats, 
        totalAllocations = allocated_bytes stats 
    }

-- | Print performance metrics in a human-readable format
printMetrics :: PerformanceMetrics -> IO ()
printMetrics metrics = do
    putStrLn "\nPerformance Metrics:"
    putStrLn "------------------"
    printf "Computation Time: %.2f seconds\n" (computationTime metrics)
    let memoryMB :: Double
        memoryMB = fromIntegral (memoryUsage metrics) / 1e6 -- memoryUsage is Word64 now
    printf "Memory Usage: %.2f MB\n" memoryMB
    printf "Parallelization Efficiency: %.1f%%\n" (parallelizationEfficiency metrics * 100)
    printf "GC Time: %.2f seconds\n" (gcTime metrics)
    printf "GC Collections: %u\n" (gcCount metrics) 
    printf "Total Allocations: %llu\n" (totalAllocations metrics) -- Use %llu for Word64

-- | Track execution time of an action
trackExecutionTime :: String -> IO a -> IO a
trackExecutionTime label action = do
    startTime <- getCurrentTime
    startCPUTime <- getCPUTime
    result <- action
    endTime <- getCurrentTime
    endCPUTime <- getCPUTime
    let duration :: Double
        duration = realToFrac $ diffUTCTime endTime startTime
        cpuDuration :: Double
        cpuDuration = fromIntegral (endCPUTime - startCPUTime) / 1e12
    printf "%s took %.2f seconds (CPU: %.2f seconds)\n" label duration cpuDuration
    return result

-- | Track memory usage before and after an action
trackMemoryUsage :: String -> IO a -> IO a
trackMemoryUsage label action = do
    performGC
    startStats <- getRTSStats
    result <- action
    performGC
    endStats <- getRTSStats
    let memoryUsed :: Double
        memoryUsed = fromIntegral (allocated_bytes endStats - allocated_bytes startStats) / 1e6
    printf "%s memory usage: %.2f MB\n" label memoryUsed
    return result 