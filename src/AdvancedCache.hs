{-# LANGUAGE OverloadedStrings #-}

module AdvancedCache (
    Cache,
    newCache,
    getCached,
    putCached,
    withCache,
    memoize,
    getCacheStats,
    clearCache,
    resizeCache
) where

import qualified Data.Map as Map
import Control.Concurrent.STM
import System.Mem (performGC)
import Control.Exception (bracket_)

-- | A thread-safe cache with automatic cleanup
data Cache k v = Cache {
    cacheMap :: TVar (Map.Map k v),
    maxSize :: Int,
    hits :: TVar Int,
    misses :: TVar Int,
    lastCleanup :: TVar Int  -- Track last cleanup time
}

-- | Create a new cache with a maximum size
newCache :: Int -> IO (Cache k v)
newCache size = do
    m <- newTVarIO Map.empty
    h <- newTVarIO 0
    miss <- newTVarIO 0
    lastCleanupTime <- newTVarIO 0
    return $ Cache m size h miss lastCleanupTime

-- | Get a value from the cache
getCached :: Ord k => Cache k v -> k -> IO (Maybe v)
getCached cache key = atomically $ do
    m <- readTVar (cacheMap cache)
    case Map.lookup key m of
        Just v -> do
            modifyTVar' (hits cache) (+1)
            return $ Just v
        Nothing -> do
            modifyTVar' (misses cache) (+1)
            return Nothing

-- | Put a value in the cache
putCached :: Ord k => Cache k v -> k -> v -> IO ()
putCached cache key value = atomically $ do
    m <- readTVar (cacheMap cache)
    let evaluatedValue = value `seq` value
        newMap = Map.insert key evaluatedValue m
    if Map.size newMap > maxSize cache
        then do
            -- Remove oldest entries if cache is full
            let trimmedMap = Map.fromList $ take (maxSize cache) $ Map.toList newMap
            writeTVar (cacheMap cache) trimmedMap
        else
            writeTVar (cacheMap cache) newMap

-- | Run an action with cache cleanup
withCache :: Cache k v -> IO a -> IO a
withCache cache action = bracket_ (return ()) cleanup action
  where
    cleanup = do
        hitCount <- readTVarIO (hits cache)
        missCount <- readTVarIO (misses cache)
        lastCleanupTime <- readTVarIO (lastCleanup cache)
        let
            rate :: Double
            rate = if (hitCount + missCount) == 0 then 0.0 else fromIntegral hitCount / fromIntegral (hitCount + missCount)
            currentTime = hitCount + missCount
            timeSinceLastCleanup = currentTime - lastCleanupTime
        
        -- Clean up if hit rate is low or it's been a while since last cleanup
        if rate < 0.5 || timeSinceLastCleanup > 10000
            then do
                atomically $ do
                    writeTVar (cacheMap cache) Map.empty
                    writeTVar (lastCleanup cache) currentTime
                performGC
            else return ()

-- | Memoize a function with caching
memoize :: Ord k => Cache k v -> (k -> IO v) -> k -> IO v
memoize cache f key = do
    maybeValue <- getCached cache key
    case maybeValue of
        Just value -> return value
        Nothing -> do
            value <- f key
            putCached cache key value
            return value

-- | Cache statistics
data CacheStats = CacheStats {
    cacheHits :: Int,
    cacheMisses :: Int,
    cacheSize :: Int,
    hitRate :: Double
} deriving (Show)

-- | Get cache statistics
getCacheStats :: Cache k v -> IO CacheStats
getCacheStats cache = atomically $ do
    h <- readTVar (hits cache)
    m <- readTVar (misses cache)
    s <- Map.size <$> readTVar (cacheMap cache)
    let rate = if (h + m) == 0 then 0.0 else fromIntegral h / fromIntegral (h + m)
    return $ CacheStats h m s rate

-- | Clear the cache
clearCache :: Cache k v -> IO ()
clearCache cache = atomically $ do
    writeTVar (cacheMap cache) Map.empty
    writeTVar (lastCleanup cache) 0

-- | Resize the cache
resizeCache :: Ord k => Cache k v -> Int -> IO ()
resizeCache cache newSize = atomically $ do
    m <- readTVar (cacheMap cache)
    let newMap = Map.fromList $ take newSize $ Map.toList m
    writeTVar (cacheMap cache) newMap 