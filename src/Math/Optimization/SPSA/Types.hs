module Math.Optimization.SPSA.Types (
  SPSA, getLoss, getConstraint,
  LossFn, ConstraintFn,
  StoppingCriteria, stopOn, stopWhenMagDiff,
  peelA, peelC, peelD, peelAll,
  checkStop
) where

import Numeric.LinearAlgebra (Vector, norm2)
import Control.Monad.State (State, get, put)

type LossFn = Vector Double -> Double
type ConstraintFn = Vector Double -> Vector Double

-- | A stopping criteria is a function of current iteration number, last iteration theta, and current theta
type StoppingCriteria = Int -> Vector Double -> Vector Double -> Bool

-- An instance of the SPSA optimization algorithm.
-- Initialize with all the parameters as object instantiation.
data SPSA = SPSA {
  iter :: Int,
  loss :: LossFn,
  constraint :: ConstraintFn,
  ak, ck :: [Double],
  stop :: StoppingCriteria,
  delta :: [Vector Double] -- a stream of random perturbation vectors
}

getLoss :: State SPSA LossFn
getLoss = get >>= return . loss

getConstraint :: State SPSA ConstraintFn
getConstraint = get >>= return . constraint

stopOn :: Int -> StoppingCriteria
stopOn n i _ _ = i >= n

stopWhenMagDiff :: Int -> Double -> StoppingCriteria
stopWhenMagDiff maxn diff i lst cur
  | i >= maxn = True
  | norm2 (cur - lst) < diff = True
  | otherwise = False


mkUnconstrainedSPSA :: Int -> LossFn -> [Double] -> [Double] -> Int -> [Vector Double] -> SPSA
mkUnconstrainedSPSA seed lss a c n pd = SPSA{loss=lss, constraint=id, ak=a, ck=c, delta=pd}

checkStop :: Vector Double -> Vector Double -> State SPSA Bool
checkStop lst cur = do
  spsa <- get
  let iteration = (iter spsa) + 1
  put spsa { iter = iteration }
  return $ (stop spsa) iteration lst cur


peelA :: State SPSA Double
peelA = peel ak (\as -> get >>= \spsa -> put spsa { ak = as })

peelC :: State SPSA Double
peelC = peel ck (\cs -> get >>= \spsa -> put spsa { ck = cs })

peelD :: State SPSA (Vector Double)
peelD = peel delta (\ds -> get >>= \spsa -> put spsa { delta = ds })

peelAll :: State SPSA (Double, Double, Vector Double)
peelAll = do
  a <- peelA
  c <- peelC
  d <- peelD
  return (a, c, d)

peel :: (SPSA -> [a]) -> ([a] -> State SPSA ()) -> State SPSA a
peel getter setter = do
  spsa <- get
  let sq = getter spsa
  let ([nxt],rst) = splitAt 1 sq
  setter rst
  return nxt