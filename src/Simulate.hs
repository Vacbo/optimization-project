{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Simulate (
    generateValidWeights,
    portfolioReturn,
    portfolioVolatility,
    calculateSharpeRatio,
    simulatePortfolioWithStocks,
    simulatePortfolios,
    -- findBestPortfolio, -- No longer used by Lib.hs, consider removing if not used elsewhere
    combinations,
    transposeMatrix,
    calculateCovarianceMatrix,
    selectAssetColumns,
    selectSubCovarianceMatrix,
    nCk
) where

import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle as S
import qualified Data.Vector.Fusion.Bundle.Monadic as SM
import Data.List (length, tails, maximumBy)
import Data.Text (Text)
-- import Control.Concurrent.Async (mapConcurrently) -- Unused
import System.Random (randomR, StdGen, mkStdGen, randomIO)
import Prelude hiding (length)
-- import Data.Maybe (listToMaybe) -- Unused
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Control.Monad (replicateM, foldM)

{-# INLINE portfolioReturn #-}
{-# INLINE portfolioVolatility #-}
{-# INLINE calculateSharpeRatio #-}
{-# INLINE weightedDot #-}

-- Helper to generate N gamma values and the final generator state
generateNGammas :: Int -> Double -> StdGen -> (V.Vector Double, StdGen)
generateNGammas n alpha initialGen =
  go n initialGen []
  where
    go 0 g acc = (V.fromList (reverse acc), g)
    go k g acc = 
      let (x, g') = gammaRandom alpha g
      in go (k-1) g' (x:acc)

-- | Generate all combinations of k elements from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs
  | n > length xs = []
  | otherwise = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

-- | Generate a gamma random variable (simplified for alpha = 1.0)
gammaRandom :: Double -> StdGen -> (Double, StdGen)
gammaRandom alpha gen = 
  let (u, gen') = randomR (0, 1) gen
  in (-log u / alpha, gen')  -- For alpha = 1.0, this gives exponential distribution

-- | Generate weights using Dirichlet distribution (more efficient version)
generateDirichletWeights :: Int -> Double -> StdGen -> (V.Vector Double, StdGen)
generateDirichletWeights n alpha gen = 
  let (gammas, gen') = generateNGammas n alpha gen -- Use the helper
      total = V.sum gammas
      -- Handle division by zero if sum is zero (though unlikely with gamma)
      invTotal = if total == 0 then 0 else 1.0 / total 
  in (V.map (* invTotal) gammas, gen')

-- | Generate valid weights with adaptive alpha and a retry limit.
-- Returns Nothing if unable to generate valid weights within the limit.
generateValidWeights :: Int -> Int -> IO (Maybe (V.Vector Double))
generateValidWeights n maxAttempts
  | n <= 0 = return Nothing  -- Invalid asset count
  | maxAttempts <= 0 = return Nothing -- Base case: exceeded attempts
  | otherwise = do
      seed <- randomIO
      let initialGen = mkStdGen seed
      let tryWithAlpha alpha attempts currGen =
            if attempts <= 0 
            then Nothing  -- Exhausted attempts
            else
              let (weights, nextGen) = generateDirichletWeights n alpha currGen
                  isValid = V.all (<= 0.2) weights
              in if isValid
                 then Just weights
                 else tryWithAlpha (alpha * 1.5) (attempts - 1) nextGen  -- Increase alpha to make weights more uniform
      
      -- Start with a lower alpha for diverse weights, but not too low
      -- First try with initial alpha, then progressively increase to make weights more uniform
      case tryWithAlpha 0.4 maxAttempts initialGen of
        Just w -> return (Just w)
        Nothing -> return Nothing

-- | Optimized dot product for portfolio calculations
weightedDot :: V.Vector Double -> V.Vector Double -> Double
weightedDot !v1 !v2 = 
    -- Direct dot product without temporary vectors
    V.foldl' (+) 0 $ V.zipWith (*) v1 v2

-- | Calculate portfolio return using cached mean returns (optimized)
portfolioReturn :: V.Vector Double -> V.Vector Double -> Double
portfolioReturn !weights !returns = weightedDot weights returns

-- | Calculate portfolio volatility using cached covariance matrix (optimized)
portfolioVolatility :: V.Vector Double -> V.Vector (V.Vector Double) -> Double
portfolioVolatility !weights !covMatrix =
    let !weightedCov = V.map (\row -> weightedDot weights row) covMatrix
        !variance = weightedDot weights weightedCov
    in sqrt variance

-- | Calculate Sharpe Ratio
calculateSharpeRatio :: Double -> Double -> Double -> Double
calculateSharpeRatio !meanReturn !volatility !riskFreeRate =
  if volatility <= 0.00001 then 0 else (meanReturn - riskFreeRate) / volatility

-- Function to calculate the covariance matrix from a returns matrix (assets in columns)
calculateCovarianceMatrix :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
calculateCovarianceMatrix returnsMatrixByCol =
  let numAssets = V.length returnsMatrixByCol
      numDays = if numAssets > 0 then V.length (returnsMatrixByCol V.! 0) else 0
      
      -- Calculate means efficiently
      means = V.map (\assetReturns -> 
                      V.foldl' (+) 0 assetReturns / fromIntegral numDays)
                    returnsMatrixByCol
      
      -- Calculate covariance directly
      covariance i j =
        let assetI = returnsMatrixByCol V.! i
            assetJ = returnsMatrixByCol V.! j
            meanI = means V.! i
            meanJ = means V.! j
            -- Inline the calculation without temporary vectors
            sumProd = V.foldl' (\acc (xi, xj) -> 
                            acc + (xi - meanI) * (xj - meanJ))
                     0 $ V.zip assetI assetJ
        in sumProd / fromIntegral (numDays - 1)  -- Sample covariance
      
  in V.generate numAssets $ \i ->
       V.generate numAssets $ \j -> covariance i j

-- Function to transpose a matrix (Vector of Vectors)
transposeMatrix :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
transposeMatrix matrix
  | V.null matrix || V.null (matrix V.! 0) = V.empty
  | otherwise = 
      let numRows = V.length matrix
          numCols = V.length (matrix V.! 0)
      in V.generate numCols $ \j -> 
           V.generate numRows $ \i -> (matrix V.! i) V.! j

-- Function to select specific columns (assets) from a returns matrix (days x assets)
selectAssetColumns :: V.Vector (V.Vector Double) -> [Int] -> V.Vector (V.Vector Double)
selectAssetColumns returnsMatrixByRow assetIndices
  | V.null returnsMatrixByRow = V.empty
  | otherwise = 
      V.map (\dayReturns -> V.fromList [dayReturns V.! i | i <- assetIndices]) returnsMatrixByRow

-- Function to select a sub-matrix from a covariance matrix based on asset indices
selectSubCovarianceMatrix :: V.Vector (V.Vector Double) -> [Int] -> V.Vector (V.Vector Double)
selectSubCovarianceMatrix fullCovMatrix assetIndices =
  V.fromList $ map (\i -> V.fromList [ (fullCovMatrix V.! i) V.! j | j <- assetIndices]) assetIndices

-- | Simulate a single portfolio using cached metrics.
-- Returns Maybe tuple, signalling failure if weights couldn't be generated.
simulatePortfolioWithStocks :: Int -- Max weight generation attempts
                          -> V.Vector Text 
                          -> V.Vector (V.Vector Double) 
                          -> V.Vector (V.Vector Double) 
                          -> IO (Maybe (Double, Double, Double, V.Vector Double))
simulatePortfolioWithStocks maxWeightAttempts stocks returnsMatrix covMatrix = do
  maybeWeights <- generateValidWeights (V.length stocks) maxWeightAttempts
  case maybeWeights of
    Nothing -> return Nothing -- Failed to generate valid weights
    Just !weights -> do
      -- Calculate all metrics in one pass with strict evaluation
      let !numDays = V.length returnsMatrix
          !sumDailyReturns = V.foldl' (\acc dayReturns -> 
                              acc + portfolioReturn weights dayReturns) 0.0 returnsMatrix
          !meanDailyReturn = if numDays == 0 then 0.0 else sumDailyReturns / fromIntegral numDays
          !dailyVolatility = portfolioVolatility weights covMatrix
          -- Constants
          !annualScaleFactor = 252.0
          !sqrtAnnualFactor = sqrt annualScaleFactor
          -- Annualized metrics 
          !annualizedReturn = meanDailyReturn * annualScaleFactor
          !annualizedVolatility = dailyVolatility * sqrtAnnualFactor
          !sharpe = calculateSharpeRatio annualizedReturn annualizedVolatility 0.0
      
      -- Return fully evaluated result
      return $! Just $! (annualizedReturn, annualizedVolatility, sharpe, weights)

-- | Simulate multiple portfolios, now using batch processing for better cache efficiency
simulatePortfolios :: Int -- nPortfolios
                   -> Int -- Max weight generation attempts per simulation
                   -> V.Vector Text 
                   -> V.Vector (V.Vector Double) 
                   -> V.Vector (V.Vector Double) 
                   -> IO (Maybe (Double, Double, Double, V.Vector Double)) -- Best valid result
simulatePortfolios nPortfolios maxWeightAttempts stocks returnsMatrix covMatrix
    | nPortfolios <= 0 = return Nothing
    | otherwise = do
        -- Shared values for all simulations
        let !numDays = V.length returnsMatrix
            !annualScaleFactor = 252.0 :: Double
            !sqrtAnnualFactor = sqrt annualScaleFactor
        
        -- Process portfolios incrementally, keeping track of best result
        foldM (\currentBest simIndex -> do
            -- Try to generate valid weights
            maybeWeights <- generateValidWeights (V.length stocks) maxWeightAttempts
            case maybeWeights of
                Nothing -> return currentBest
                Just !weights -> do
                    -- Calculate metrics
                    let !sumDailyReturns = V.foldl' (\acc dayReturns -> 
                                           acc + portfolioReturn weights dayReturns) 0.0 returnsMatrix
                        !meanDailyReturn = if numDays == 0 then 0.0 else sumDailyReturns / fromIntegral numDays
                        !dailyVolatility = portfolioVolatility weights covMatrix
                        -- Annualized metrics 
                        !annualizedReturn = meanDailyReturn * annualScaleFactor
                        !annualizedVolatility = dailyVolatility * sqrtAnnualFactor
                        !sharpe = calculateSharpeRatio annualizedReturn annualizedVolatility 0.0
                        -- New result
                        newResult = (annualizedReturn, annualizedVolatility, sharpe, weights)
                    
                    -- Update best result if needed
                    return $ case currentBest of
                        Nothing -> Just newResult
                        Just best@(_, _, sr1, _) ->
                            if sharpe > sr1 then Just newResult else Just best
            ) Nothing [1..nPortfolios]

-- Function to calculate binomial coefficient nCk
nCk :: Int -> Int -> Integer
nCk n k
    | k < 0 || k > n = 0
    | k == 0 || k == n = 1
    | k > n `div` 2 = nCk n (n - k) -- Optimization: C(n, k) == C(n, n-k)
    | otherwise = product [fromIntegral (n - i + 1) | i <- [1..k]] `div` product [fromIntegral i | i <- [1..k]]

-- The following functions were removed as they are no longer used:
-- simulatePortfolio, calculateExpectedReturn, calculateRisk, PortfolioMetrics data type 