{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module MemoryEfficient (
    StreamingMatrix,
    newStreamingMatrix,
    addRow,
    getRow,
    getMatrix,
    withGC
) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad (forM_)
import System.Mem (performGC)
import Control.Exception (bracket_)

-- | A streaming matrix that efficiently handles large datasets
data StreamingMatrix a = StreamingMatrix {
    matrix :: MV.IOVector (V.Vector a),
    currentSize :: MV.IOVector Int,
    maxSize :: Int
}

-- | Create a new streaming matrix with a maximum size
newStreamingMatrix :: IO (StreamingMatrix a)
newStreamingMatrix = do
    m <- MV.new 1000  -- Default initial size
    s <- MV.new 1
    MV.write s 0 0
    return $ StreamingMatrix m s 1000

-- | Add a row to the streaming matrix
addRow :: StreamingMatrix a -> V.Vector a -> IO ()
addRow sm row = do
    size <- MV.read (currentSize sm) 0
    let evaluatedRow = row `seq` row  -- Force evaluation
    if size < maxSize sm
        then do
            MV.write (matrix sm) size evaluatedRow
            MV.write (currentSize sm) 0 (size + 1)
        else do
            -- If matrix is full, perform GC and resize
            performGC
            newMatrix <- MV.new (maxSize sm * 2)
            forM_ [0..size-1] $ \i -> do
                currentRow <- MV.read (matrix sm) i
                MV.write newMatrix i currentRow
            MV.write newMatrix size evaluatedRow
            MV.write (currentSize sm) 0 (size + 1)

-- | Get a row from the streaming matrix
getRow :: StreamingMatrix a -> Int -> IO (V.Vector a)
getRow sm i = MV.read (matrix sm) i

-- | Get the entire matrix as a vector of vectors
getMatrix :: StreamingMatrix a -> IO (V.Vector (V.Vector a))
getMatrix sm = do
    size <- MV.read (currentSize sm) 0
    V.generateM size $ \i -> MV.read (matrix sm) i

-- | Run an action with explicit garbage collection
withGC :: IO a -> IO a
withGC action = bracket_ (return ()) performGC action 