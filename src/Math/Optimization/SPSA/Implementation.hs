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

import System.Random (getStdGen, randomRs, Random(..))
import Data.Word (Word8, Word32)
import Data.List (genericSplitAt)

import Math.Optimization.SPSA.Types (SPSA(..),UDouble(..),Exponent(..),UInt(..))

-----------------
-- Types
-----------------


mkUnconstrainedSPSA :: ([Double]->Double) -> [Double] -> [Double] -> UInt -> IO SPSA
mkUnconstrainedSPSA loss ak ck n = do
  pd <- bernoulli n
  return SPSA{loss=loss, constraint=id, ak=ak, ck=ck, delta=pd}

-----------------
-- Main Functions
-----------------

optimize :: SPSA -> UInt -> [Double] -> [Double]
optimize spsa (UInt rounds) t0 = foldl optimize t0 (take rounds $ zip3 (ak spsa) (ck spsa) (delta spsa))
  where
    constrainF = constraint spsa
    lossF = loss spsa
    optimize t (a,c,d) = constrainF $ [tk - a * gk | (tk,gk) <- zip t g]
      where
        ya = lossF [tk + c * dk | (tk,dk) <- zip t d]
        yb = lossF [tk - c * dk | (tk,dk) <- zip t d]
        g = [(ya - yb) / (2 * c * dk) | dk <- d]


-----------------
-- Gain Sequences
-----------------

standardAk :: UDouble -> UInt -> Exponent -> [Double]
standardAk (UDouble a) (UInt aA) (Exponent alpha) = map (\k -> a / (k + 1 + (fromIntegral aA)) ** alpha) [1..]

standardCk :: UDouble -> Exponent -> [Double]
standardCk c gamma = standardAk c 0 gamma

semiautomaticTuning :: UDouble -> UDouble -> ([Double],[Double])
semiautomaticTuning a c = (standardAk a 0 0.602, standardCk c 0.101)

-----------------
-- Distributions
-----------------

bernoulli :: UInt -> IO [[Double]]
bernoulli n = do
  stdgen <- getStdGen
  rands <- return (randomRs (0,1) stdgen :: [Word8])
  berns <- return $ map (\i -> 2 * ((fromIntegral i) - 0.5)) rands
  return $ accumEvery n berns

-----------------
-- Helpers
-----------------

accumEvery :: Integral i => i -> [a] -> [[a]]
accumEvery n xs = map fst $ iterate (genericSplitAt n . snd) (genericSplitAt n xs)