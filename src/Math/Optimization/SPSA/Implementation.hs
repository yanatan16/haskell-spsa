-- Package spsa provides the Simultaneous Perturbation Stochastic Approximation method.
--
-- Much of the notation is taken from Introduction To Stochastic Search and Optimization
-- (ISSO), James Spall's book on Stochastic Optimization. (published by Wiley 2003)
--
module Math.Optimization.SPSA.Implementation (
  optimize,
  standardAk,
  standardCk,
  semiautomaticTuning,
  bernoulli,
  mkUnconstrainedSPSA
) where

import System.Random (mkStdGen,randoms)
import Numeric.LinearAlgebra (Vector,scale,scaleRecip,randomVector,RandDist(Uniform))

import Math.Optimization.SPSA.Types (LossFn,SPSA(..))

-----------------
-- Types
-----------------


mkUnconstrainedSPSA :: Int -> LossFn -> [Double] -> [Double] -> Int -> SPSA
mkUnconstrainedSPSA seed lss a c n = SPSA{loss=lss, constraint=id, ak=a, ck=c, delta=bernoulli seed n}

-----------------
-- Main Functions
-----------------

optimize :: SPSA -> Int -> Vector Double -> Vector Double
optimize spsa rounds t0 = foldl opt t0 (take rounds $ zip3 (ak spsa) (ck spsa) (delta spsa))
  where
    constrainF = constraint spsa
    lossF = loss spsa
    opt t (a,c,d) = constrainF (t - (scale a g))
      where
        cd = scale c d
        ya = lossF (t + cd)
        yb = lossF (t - cd)
        g = scaleRecip ((ya - yb) / 2) cd


-----------------
-- Gain Sequences
-----------------

standardAk :: Double -> Int -> Double -> [Double]
standardAk a aA alpha = map (\k -> a / (k + 1 + (fromIntegral aA)) ** alpha) [1..]

standardCk :: Double -> Double -> [Double]
standardCk c gamma = standardAk c 0 gamma

semiautomaticTuning :: Double -> Double -> ([Double],[Double])
semiautomaticTuning a c = (standardAk a 0 0.602, standardCk c 0.101)

-----------------
-- Distributions
-----------------

bernoulli :: Int -> Int -> [Vector Double]
bernoulli seed n = map (toBernoulli . mkVec) seeds
  where
    toBernoulli vec = (vec - 0.5) / (abs $ vec - 0.5)
    mkVec s = (randomVector s Uniform n)
    seeds = (randoms $ mkStdGen seed :: [Int])