{-# LANGUAGE OverloadedStrings #-}

module WorkStealing (
    WorkStealingQueue,
    newQueue,
    processItems,
    push,
    pop,
    steal,
    processWithWorkStealing
) where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad (replicateM, forM, forM_)
import GHC.Conc (numCapabilities)

-- | Thread-safe queue for work stealing
data WorkStealingQueue a = WorkStealingQueue {
    queue :: TVar [a],
    size :: TVar Int
}

-- | Create a new work stealing queue
newQueue :: IO (WorkStealingQueue a)
newQueue = do
    q <- newTVarIO []
    s <- newTVarIO 0
    return $ WorkStealingQueue q s

-- | Push an item to the queue
push :: WorkStealingQueue a -> a -> STM ()
push ws item = do
    items <- readTVar (queue ws)
    writeTVar (queue ws) (item : items)
    modifyTVar' (size ws) (+1)

-- | Pop an item from the queue
pop :: WorkStealingQueue a -> STM (Maybe a)
pop ws = do
    items <- readTVar (queue ws)
    case items of
        [] -> return Nothing
        (x:xs) -> do
            writeTVar (queue ws) xs
            modifyTVar' (size ws) (subtract 1)
            return $ Just x

-- | Steal an item from another queue
steal :: WorkStealingQueue a -> STM (Maybe a)
steal ws = do
    items <- readTVar (queue ws)
    case items of
        [] -> return Nothing
        (x:xs) -> do -- Handles both one-element and multi-element lists
            writeTVar (queue ws) xs
            modifyTVar' (size ws) (subtract 1)
            return $ Just x

-- | Process items using work stealing with parallel evaluation
processItems :: WorkStealingQueue a -> Int -> (a -> IO b) -> IO [b]
processItems wsQueue _numItems processFn = do -- Marked _numItems as unused
    let numWorkers = numCapabilities
    results <- newTVarIO []
    
    -- Create worker threads
    workers <- forM [0..numWorkers-1] $ \_ -> async $ do
        let workLoop = do
                -- Try to get work from queue
                maybeItem <- atomically $ pop wsQueue
                case maybeItem of
                    Just item -> do
                        result <- processFn item
                        let evaluatedResult = result `seq` result  -- Force evaluation
                        atomically $ modifyTVar' results (evaluatedResult:)
                        workLoop
                    Nothing -> return ()
        
        workLoop
    
    -- Wait for all workers and collect results
    mapM_ wait workers
    readTVarIO results

-- | Process items using work stealing with multiple queues and parallel evaluation
processWithWorkStealing :: (a -> IO b) -> [a] -> IO [b]
processWithWorkStealing f items = do
    let numWorkers = numCapabilities
    queues <- replicateM numWorkers newQueue
    
    -- Distribute initial work
    forM_ (zip [0..] items) $ \(i, item) -> do
        let queueIndex = i `mod` numWorkers
        atomically $ push (queues !! queueIndex) item
    
    -- Create worker threads
    workers <- forM [0..numWorkers-1] $ \workerId -> async $ do
        let myQueue = queues !! workerId
        results <- newTVarIO []
        
        let processItem item = do
                result <- f item
                let evaluatedResult = result `seq` result  -- Force evaluation
                atomically $ modifyTVar' results (evaluatedResult:)
        
        let workLoop = do
                -- Try to get work from my queue
                maybeItem <- atomically $ pop myQueue
                case maybeItem of
                    Just item -> do
                        processItem item
                        workLoop
                    Nothing -> do
                        -- Try to steal work from other queues
                        let otherQueues = [q | (i, q) <- zip [0..] queues, i /= workerId]
                        maybeStolenItem <- atomically $ foldr (\q acc -> do
                            acc' <- acc
                            case acc' of
                                Just _ -> return acc'
                                Nothing -> steal q) (return Nothing) otherQueues
                        case maybeStolenItem of
                            Just item -> do
                                processItem item
                                workLoop
                            Nothing -> return ()
        
        workLoop
        readTVarIO results
    
    -- Wait for all workers and collect results
    results <- mapM wait workers
    return $ concat results 