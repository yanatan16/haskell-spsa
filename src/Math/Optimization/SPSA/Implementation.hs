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
  bernoulli
) where

import Control.Monad.State (State, MonadState(get,put))
import System.Random (mkStdGen,randoms)
import Numeric.LinearAlgebra (Vector,scale,scaleRecip,randomVector,RandDist(Uniform))

import Math.Optimization.SPSA.Types

-----------------
-- Main Functions
-----------------

--optimize :: SPSA -> Int -> Vector Double -> Vector Double
--optimize spsa rounds t0 = foldl opt t0 (take rounds $ zip3 (ak spsa) (ck spsa) (delta spsa))
--  where
--    constrainF = constraint spsa
--    lossF = loss spsa
--    opt t (a,c,d) = constrainF (t - (scale a g))
--      where
--        cd = scale c d
--        ya = lossF (t + cd)
--        yb = lossF (t - cd)
--        g = scaleRecip ((ya - yb) / 2) cd

optimizeSingle :: Vector Double -> State SPSA (Vector Double)
optimizeSingle t = do
  (a, c, d) <- peelAll
  lossF <- getLoss
  constrainF <- getConstraint
  let cd = c `scale` d
  let ya = lossF (t + cd)
  let yb = lossF (t - cd)
  let grad = ((ya - yb) / 2) `scaleRecip` cd
  return $ constrainF $ t - (a `scale` grad)

optimize :: Vector Double -> State SPSA (Vector Double)
optimize t = do
  t' <- optimizeSingle t
  stop <- checkStop t t'
  if stop then return t' else optimize t'


-----------------
-- Gain Sequences
-----------------

standardAk :: Double -> Int -> Double -> [Double]
standardAk a aA alpha = map (\k -> a / (k + 1 + (fromIntegral aA)) ** alpha) [1..]

standardCk :: Double -> Double -> [Double]
standardCk c gamma = standardAk c 0 gamma

-- | Semiautomatic tuning for the gain sequences
-- | c should be about the standard deviation of a measurement
-- | a is set how much you want the first iteration to move
semiautomaticTuning :: Int -> Double -> Double -> ([Double],[Double])
semiautomaticTuning iterations a c = (standardAk a (iterations `quot` 10) 0.602, standardCk c 0.101)

--smarterTuning :: LossFn -> Int -> ()

-----------------
-- Distributions
-----------------

bernoulli :: Int -> Int -> [Vector Double]
bernoulli seed n = map (toBernoulli . mkVec) seeds
  where
    toBernoulli vec = (vec - 0.5) / (abs $ vec - 0.5)
    mkVec s = (randomVector s Uniform n)
    seeds = (randoms $ mkStdGen seed :: [Int])