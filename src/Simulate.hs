{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.List (length, tails)
import Data.Text (Text)
-- import Control.Concurrent.Async (mapConcurrently) -- Unused
import System.Random (randomR, StdGen, mkStdGen, randomIO)
import Prelude hiding (length)
-- import Data.Maybe (listToMaybe) -- Unused
import Control.Monad (foldM)
import Data.Maybe (Maybe(..), catMaybes)

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

-- | Generate weights using Dirichlet distribution
generateDirichletWeights :: Int -> Double -> StdGen -> (V.Vector Double, StdGen)
generateDirichletWeights n alpha gen = 
  let (gammas, gen') = foldl (\(acc, g) _ -> 
        let (x, g') = gammaRandom alpha g
        in (acc ++ [x], g')) ([], gen) [1..n]
      total = sum gammas
  in (V.fromList $ map (/ total) gammas, gen')

-- | Generate valid weights with adaptive alpha and a retry limit.
-- Returns Nothing if unable to generate valid weights within the limit.
generateValidWeights :: Int -> Int -> IO (Maybe (V.Vector Double))
generateValidWeights n maxAttempts
  | maxAttempts <= 0 = return Nothing -- Base case: exceeded attempts
  | otherwise = do
      seed <- randomIO
      let gen = mkStdGen seed
      -- Try initial alpha
      let initialAlpha = 0.5 
      let (weights, gen') = generateDirichletWeights n initialAlpha gen
      
      if V.all (<= 0.2) weights
        then return (Just weights)
        else do
          -- If weights are invalid, try with a different alpha
          let (weights', _) = generateDirichletWeights n (initialAlpha * 2) gen'
          if V.all (<= 0.2) weights'
            then return (Just weights')
            -- Decrement attempts and recurse
            else generateValidWeights n (maxAttempts - 1) 

-- | Calculate portfolio return using cached mean returns
portfolioReturn :: V.Vector Double -> V.Vector Double -> Double
portfolioReturn weights returns =
  let dailyReturn = V.sum $ V.zipWith (*) weights returns
  in dailyReturn  -- Return daily return, annualization happens in simulatePortfolio

-- | Calculate portfolio volatility using cached covariance matrix
portfolioVolatility :: V.Vector Double -> V.Vector (V.Vector Double) -> Double
portfolioVolatility weights covMatrix =
  let weightedCov = V.map (V.sum . V.zipWith (*) weights) covMatrix
      variance = V.sum $ V.zipWith (*) weights weightedCov
      dailyVolatility = sqrt variance
  in dailyVolatility  -- Return daily volatility, annualization happens in simulatePortfolio

-- | Calculate Sharpe Ratio
calculateSharpeRatio :: Double -> Double -> Double -> Double
calculateSharpeRatio meanReturn volatility riskFreeRate =
  if volatility == 0 then 0 else (meanReturn - riskFreeRate) / volatility

-- Function to calculate the covariance matrix from a returns matrix (assets in columns)
calculateCovarianceMatrix :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
calculateCovarianceMatrix returnsMatrixByCol =
  let numAssets = V.length returnsMatrixByCol
      numDays = V.length (returnsMatrixByCol V.! 0)
      means = V.map (\assetReturns -> V.sum assetReturns / fromIntegral numDays) returnsMatrixByCol
      centeredReturns = V.generate numAssets $ \j ->
                          V.map (\ret -> ret - (means V.! j)) (returnsMatrixByCol V.! j)
      covariance i j =
        let sumProd = V.sum $ V.zipWith (*) (centeredReturns V.! i) (centeredReturns V.! j)
        in sumProd / fromIntegral (numDays - 1) -- Use sample covariance
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
    Just weights -> do
      let portfolioReturns = V.map (portfolioReturn weights) returnsMatrix
          meanDailyReturn = V.sum portfolioReturns / fromIntegral (V.length portfolioReturns)
          dailyVolatility = portfolioVolatility weights covMatrix
          -- Annualize returns and volatility
          annualizedReturn = meanDailyReturn * 252
          annualizedVolatility = dailyVolatility * sqrt 252
          sharpe = calculateSharpeRatio annualizedReturn annualizedVolatility 0.0
      -- Force evaluation
      let !ar = annualizedReturn
          !av = annualizedVolatility
          !s = sharpe
          !w = weights
      return (Just (ar, av, s, w))

-- | Simulate multiple portfolios sequentially, returning only the best valid result found.
simulatePortfolios :: Int -- nPortfolios
                   -> Int -- Max weight generation attempts per simulation
                   -> V.Vector Text 
                   -> V.Vector (V.Vector Double) 
                   -> V.Vector (V.Vector Double) 
                   -> IO (Maybe (Double, Double, Double, V.Vector Double)) -- Best valid result
simulatePortfolios nPortfolios maxWeightAttempts stocks returnsMatrix covMatrix
  | nPortfolios <= 0 = return Nothing
  | otherwise = do
      -- Run simulations, collecting only valid results (Just values)
      results <- catMaybes <$> mapM (\_ -> simulatePortfolioWithStocks maxWeightAttempts stocks returnsMatrix covMatrix) [1..nPortfolios]

      -- Find the best among the valid results
      case results of
          [] -> return Nothing -- No valid simulations succeeded
          (r:rs) -> return $ Just $ foldl findBetter r rs
    where
      findBetter best@(_, _, bestSharpe, _) current@(_, _, currentSharpe, _) =
        if currentSharpe > bestSharpe then current else best

-- Function to calculate binomial coefficient nCk
nCk :: Int -> Int -> Integer
nCk n k
    | k < 0 || k > n = 0
    | k == 0 || k == n = 1
    | k > n `div` 2 = nCk n (n - k) -- Optimization: C(n, k) == C(n, n-k)
    | otherwise = product [fromIntegral (n - i + 1) | i <- [1..k]] `div` product [fromIntegral i | i <- [1..k]]

-- The following functions were removed as they are no longer used:
-- simulatePortfolio, calculateExpectedReturn, calculateRisk, PortfolioMetrics data type 