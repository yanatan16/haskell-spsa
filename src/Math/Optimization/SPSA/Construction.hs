module Math.Optimization.SPSA.Construction (
  -- Construction of the SPSA type through the State monad
  standardGainAk,
  standardGainCk,
  semiautomaticTuning,
  bernoulli
) where

import System.Random (mkStdGen, randoms)
import Numeric.LinearAlgebra (Vector,randomVector,RandDist(Uniform))

import Math.Optimization.SPSA.Types (StateSPSA, setGainA, setGainC)

-----------------
-- Gain Sequences
-----------------

-- | Standard gain sequence for a_k = a / (k + 1 + A) ^ alpha
-- | a is the most important and variable tuning parameter. Start with it set to get desired change in first iteration.
-- | A is usually set to 10% of expected iterations
-- | alpha is usually set to 0.602, but is valid up to 1.0 (the optimal for infinite iterations)
standardGainAk :: Double -> Int -> Double -> [Double]
standardGainAk a aA alpha = map (\k -> a / (k + 1 + (fromIntegral aA)) ** alpha) [1..]

-- | Standard gain sequence for c_k
standardGainCk :: Double -> Double -> [Double]
standardGainCk c gamma = standardGainAk c 0 gamma

-- | Set semiautomatic tuning for the gain sequences
-- | c should be about the standard deviation of a measurement
-- | a is set how much you want the first iteration to move
-- | A is set to be 10% of expected iterations
-- | alpha and gamma are set to the lowest values for best finite iteration performance (not infinitely optimal)
semiautomaticTuning :: Int -> Double -> Double -> StateSPSA ()
semiautomaticTuning iterations a c = do
  setGainA $ standardGainAk a (iterations `quot` 10) 0.602
  setGainC $ standardGainCk c 0.101

-----------------
-- Distributions
-----------------

-- | Create a bernoulli +1 / -1 distributed infinite series of perturbation vectors based on seed and dimension
bernoulli :: Int -> Int -> [Vector Double]
bernoulli seed n = map (toBernoulli . mkVec) seeds
  where
    toBernoulli vec = (vec - 0.5) / (abs $ vec - 0.5)
    mkVec s = (randomVector s Uniform n)
    seeds = (randoms $ mkStdGen seed :: [Int])