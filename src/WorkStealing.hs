{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module WorkStealing (
    processWithWorkStealing,
    processWithWorkStealingProgress
) where

import Control.Concurrent.Async
import Control.Monad (replicateM, forM, forM_, when)
import GHC.Conc (numCapabilities)
import Control.Concurrent.MVar
import Data.IORef
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan

-- | Thread-safe queue for work stealing
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar

-- | Thread-safe queue for work stealing
data Queue a = Queue {
    queueContents :: TQueue a,
    queueSize :: TVar Int
}

-- | Create a new empty work-stealing queue
newQueue :: IO (Queue a)
newQueue = do
    q <- newTQueueIO
    size <- newTVarIO 0
    return $ Queue q size

-- | Push an item onto the work-stealing queue
push :: Queue a -> a -> STM ()
push queue item = do
    writeTQueue (queueContents queue) item
    modifyTVar' (queueSize queue) (+1)

-- | Pop an item from the work-stealing queue (LIFO order)
pop :: Queue a -> STM (Maybe a)
pop queue = do
    size <- readTVar (queueSize queue)
    if size == 0
        then return Nothing
        else do
            item <- tryReadTQueue (queueContents queue)
            case item of
                Just x -> do
                    modifyTVar' (queueSize queue) (subtract 1)
                    return $ Just x
                Nothing -> return Nothing

-- | Steal an item from the work-stealing queue (FIFO order)
steal :: Queue a -> STM (Maybe a)
steal queue = do
    size <- readTVar (queueSize queue)
    if size == 0
        then return Nothing
        else do
            item <- tryReadTQueue (queueContents queue)
            case item of
                Just x -> do
                    modifyTVar' (queueSize queue) (subtract 1)
                    return $ Just x
                Nothing -> return Nothing

-- | Process tasks in parallel with work stealing
-- For better efficiency with large numbers of similar tasks,
-- we divide work into larger chunks upfront
processWithWorkStealing :: (a -> IO b) -> [a] -> IO [b]
processWithWorkStealing f items = do
    let numWorkers = numCapabilities
        totalItems = length items
        
        -- Adaptive chunk sizing based on input size and cores
        -- Use larger chunks for better performance
        itemsPerWorker = max 1 (totalItems `div` numWorkers)
        chunkSize = if totalItems < 1000
                    then max 25 (min 100 (itemsPerWorker `div` 2))
                    else max 100 (min 500 (itemsPerWorker `div` 1))
        chunks = chunksOf chunkSize items
        totalChunks = length chunks
    
    -- Create a channel for results
    resultChan <- newChan
    
    -- Track the number of chunks processed
    processedChunksRef <- newIORef 0
    
    -- Create a MVar to signal completion
    doneMVar <- newEmptyMVar
    
    -- Create workers and distribute initial work
    workerStates <- forM [0..numWorkers-1] $ \_ -> do
        workQueue <- newQueue
        return workQueue
    
    -- Distribute chunks to worker queues - round-robin
    forM_ (zip chunks (cycle [0..numWorkers-1])) $ \(chunk, workerIdx) ->
        atomically $ push (workerStates !! workerIdx) chunk
    
    -- Launch worker threads
    workers <- forM [0..numWorkers-1] $ \workerIdx -> async $ do
        let myQueue = workerStates !! workerIdx
            otherQueues = [workerStates !! i | i <- [0..numWorkers-1], i /= workerIdx]
            
            -- Process any available work
            processWork = do
                -- Try my queue first
                mChunk <- atomically $ pop myQueue
                case mChunk of
                    Just chunk -> do
                        -- Process chunk
                        results <- mapM f chunk
                        writeChan resultChan results
                        atomicModifyIORef' processedChunksRef $ \n -> (n + 1, ())
                        processWork
                    Nothing -> stealWork otherQueues
            
            -- Try to steal work from other queues
            stealWork [] = return () -- No more queues to try
            stealWork (q:qs) = do
                mChunk <- atomically $ steal q
                case mChunk of
                    Just chunk -> do
                        -- Process stolen chunk
                        results <- mapM f chunk
                        writeChan resultChan results
                        atomicModifyIORef' processedChunksRef $ \n -> (n + 1, ())
                        processWork
                    Nothing -> stealWork qs
        
        -- Start the worker
        processWork
    
    -- Monitor worker completion
    monitorThread <- async $ do
        let checkCompletion = do
                processed <- readIORef processedChunksRef
                if processed >= totalChunks
                    then putMVar doneMVar ()
                    else do
                        threadDelay 100000 -- 100ms
                        checkCompletion
        checkCompletion
    
    -- Wait for work to complete
    takeMVar doneMVar
    
    -- Cancel the workers - they might be stuck waiting for work
    mapM_ cancel workers
    cancel monitorThread
    
    -- Collect all results
    results <- replicateChan totalChunks resultChan
    return (concat results)

-- Collect a specific number of elements from a Chan
replicateChan :: Int -> Chan a -> IO [a]
replicateChan n chan = go n []
  where
    go 0 acc = return (reverse acc)
    go count acc = do
        val <- readChan chan
        go (count - 1) (val : acc)

-- Split a list into chunks of specified maximum size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Process tasks in parallel with work stealing with progress reporting
processWithWorkStealingProgress :: ((Int, Int) -> IO ()) -> (a -> IO b) -> [a] -> IO [b]
processWithWorkStealingProgress progressCallback f items = do
    let numWorkers = numCapabilities
        totalItems = length items
        
        -- Adaptive chunk sizing - use much larger chunks to reduce overhead
        -- Large chunks have been proven to work better with this workload
        itemsPerWorker = max 1 (totalItems `div` numWorkers)
        chunkSize = max 200 (min 1000 itemsPerWorker)
        chunks = chunksOf chunkSize items
        totalChunks = length chunks
    
    -- Create a channel for results
    resultChan <- newChan
    
    -- Track the number of chunks processed and items processed
    processedChunksRef <- newIORef 0
    processedItemsRef <- newIORef 0
    
    -- Create a MVar to signal completion
    doneMVar <- newEmptyMVar
    
    -- Create workers and distribute initial work
    workerStates <- forM [0..numWorkers-1] $ \_ -> do
        workQueue <- newQueue
        return workQueue
    
    -- Distribute chunks to worker queues in round-robin fashion
    forM_ (zip chunks (cycle [0..numWorkers-1])) $ \(chunk, workerIdx) ->
        atomically $ push (workerStates !! workerIdx) chunk
    
    -- Launch worker threads with progress tracking
    workers <- forM [0..numWorkers-1] $ \workerIdx -> async $ do
        let myQueue = workerStates !! workerIdx
            otherQueues = [workerStates !! i | i <- [0..numWorkers-1], i /= workerIdx]
            
            -- Process any available work
            processWork = do
                -- Try my queue first
                mChunk <- atomically $ pop myQueue
                case mChunk of
                    Just chunk -> do
                        -- Process chunk
                        results <- mapM f chunk
                        writeChan resultChan results
                        -- Update progress
                        atomicModifyIORef' processedChunksRef $ \n -> (n + 1, ())
                        let chunkItemCount = length chunk
                        atomicModifyIORef' processedItemsRef $ \n -> (n + chunkItemCount, ())
                        -- Continue with more work
                        processWork
                    Nothing -> stealWork otherQueues
            
            -- Try to steal work from other queues
            stealWork [] = return () -- No more queues to try
            stealWork (q:qs) = do
                mChunk <- atomically $ steal q
                case mChunk of
                    Just chunk -> do
                        -- Process stolen chunk
                        results <- mapM f chunk
                        writeChan resultChan results
                        -- Update progress
                        atomicModifyIORef' processedChunksRef $ \n -> (n + 1, ())
                        let chunkItemCount = length chunk
                        atomicModifyIORef' processedItemsRef $ \n -> (n + chunkItemCount, ())
                        -- Continue with more work
                        processWork
                    Nothing -> stealWork qs
        
        -- Start the worker
        processWork
    
    -- Monitor worker completion and progress
    monitorThread <- async $ do
        lastProgressUpdate <- newIORef 0
        let checkCompletion = do
                processed <- readIORef processedChunksRef
                if processed >= totalChunks
                    then putMVar doneMVar ()
                    else do
                        -- Update progress every 500ms for smoother UI
                        currentItems <- readIORef processedItemsRef
                        lastUpdate <- readIORef lastProgressUpdate
                        
                        -- Only update progress periodically to reduce overhead
                        when (currentItems - lastUpdate > totalItems `div` 100 || currentItems == totalItems) $ do
                            writeIORef lastProgressUpdate currentItems
                            progressCallback (currentItems, totalItems)
                        
                        threadDelay 500000 -- 500ms
                        checkCompletion
        checkCompletion
    
    -- Wait for work to complete
    takeMVar doneMVar
    
    -- Final progress update
    finalItems <- readIORef processedItemsRef
    progressCallback (finalItems, totalItems)
    
    -- Cancel the workers - they might be stuck waiting for work
    mapM_ cancel workers
    cancel monitorThread
    
    -- Collect all results
    results <- replicateChan totalChunks resultChan
    return (concat results) 