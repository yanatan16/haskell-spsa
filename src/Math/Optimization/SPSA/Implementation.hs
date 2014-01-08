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

import System.Random (getStdGen,randomRs)
import Data.Word (Word8)
import Data.List (genericSplitAt)
import Numeric.LinearAlgebra (Vector,scale,scaleRecip,fromList)

import Math.Optimization.SPSA.Types (LossFn,SPSA(..))

-----------------
-- Types
-----------------


mkUnconstrainedSPSA :: LossFn -> [Double] -> [Double] -> Int -> IO SPSA
mkUnconstrainedSPSA lss a c n = do
  pd <- bernoulli n
  return SPSA{loss=lss, constraint=id, ak=a, ck=c, delta=pd}

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
        _ = (a,c,d) :: (Double,Double,Vector Double)


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

bernoulli :: Int -> IO [Vector Double]
bernoulli n = do
  stdgen <- getStdGen
  rands <- return (randomRs (0,1) stdgen :: [Word8])
  berns <- return $ map (\i -> 2 * ((fromIntegral i) - 0.5)) rands
  return $ map fromList $ accumEvery n berns

-----------------
-- Helpers
-----------------

accumEvery :: Integral i => i -> [a] -> [[a]]
accumEvery n xs = map fst $ iterate (genericSplitAt n . snd) (genericSplitAt n xs)