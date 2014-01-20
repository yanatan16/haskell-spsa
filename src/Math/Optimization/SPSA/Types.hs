module Math.Optimization.SPSA.Types (
  defaultSPSA, checkSPSA,
  StateSPSA,
  getLoss, getConstraint, getStop, peelAll, getIterations,
  setLoss, setConstraint, pushStopCrit, setGainA, setGainC, setPerturbation,
  incrementIteration,
  StoppingCriteria(..), shouldStop,
  LossFn, ConstraintFn
) where

import Numeric.LinearAlgebra (Vector, norm2)
import Control.Monad.State (State, get, put)

----------------
-- SPSA Types --
----------------

-- An instance of the SPSA optimization algorithm.
-- Initialize with all the parameters as object instantiation.
data SPSA = SPSA {
  iterations :: Int,
  lossFn :: LossFn,
  constraintFn :: ConstraintFn,
  gainA, gainC :: [Double],
  stoppingCrits :: [StoppingCriteria],
  perturbation :: [Vector Double] -- a stream of random perturbation vectors
}

-- | Loss Function
type LossFn = Vector Double -> Double

-- | Constraint Function
type ConstraintFn = Vector Double -> Vector Double

-- | Make a default SPSA instance
defaultSPSA :: SPSA
defaultSPSA = SPSA {
  iterations = 0,
  lossFn = \_ -> error "No loss function implemented",
  constraintFn = id,
  gainA = [],
  gainC = [],
  perturbation = [],
  stoppingCrits = []
}

-- | Check to make sure all required fields are filled
checkSPSA :: Vector Double -> StateSPSA ()
checkSPSA t = do
  spsa <- get
  return $ lossFn spsa t
  if gainA spsa == [] then error "gain sequence a_k must be specified" else return ()
  if gainC spsa == [] then error "gain sequence c_k must be specified" else return ()
  if perturbation spsa == [] then error "perturbation vector sequence must be specified" else return ()
  if stoppingCrits spsa == [] then error "a stopping criteria must be specified" else return ()


-----------------------
-- Stopping Criteria --
-----------------------

-- | A stopping criteria is a function of current iteration number, last iteration theta, and current theta
data StoppingCriteria = Iterations Int | NormDiff Double deriving (Eq)

-- | Check to see if we should stop based on the critieria, iteration count, last and current iteration
shouldStop :: StoppingCriteria -> Int -> Vector Double -> Vector Double -> Bool
shouldStop (Iterations n) i _ _ = i >= n
shouldStop (NormDiff diff) _ lst cur = norm2 (cur - lst) < diff

-----------------
-- State Monad --
-----------------

-- The primary monadic type to be passed around
type StateSPSA = State SPSA

-- | Get an arbitrary field out of SPSA
getSPSA :: (SPSA -> a) -> StateSPSA a
getSPSA extractor = get >>= return . extractor

-- | Set an arbitrary field of SPSA
setSPSA :: (a -> SPSA -> SPSA) -> a -> StateSPSA ()
setSPSA updater val = get >>= put . updater val

-- | Get the loss function out of StateSPSA
getLoss :: StateSPSA LossFn
getLoss = getSPSA lossFn

-- | Set the loss function
setLoss :: LossFn -> StateSPSA ()
setLoss = setSPSA $ \loss spsa -> spsa { lossFn = loss }

-- | Get the constraint function out of StateSPSA
getConstraint :: StateSPSA ConstraintFn
getConstraint = getSPSA constraintFn

-- | Set the constraint function
setConstraint :: ConstraintFn -> StateSPSA ()
setConstraint = setSPSA $ \constraint spsa -> spsa { constraintFn = constraint }

-- | Get the stopping functions out of StateSPSA
getStop :: StateSPSA [StoppingCriteria]
getStop = getSPSA stoppingCrits

-- | Push a stopping criteria onto SPSA
pushStopCrit :: StoppingCriteria -> StateSPSA ()
pushStopCrit = setSPSA $ \sc spsa -> let crits = sc : stoppingCrits spsa in spsa { stoppingCrits = crits }

-- | Get the iteration count
getIterations :: StateSPSA Int
getIterations = getSPSA iterations

-- | Increment the iteration count
incrementIteration :: StateSPSA Int
incrementIteration = do
  spsa <- get
  let iter = 1 + (iterations spsa)
  put spsa { iterations = iter }
  return iter

-- | Peel a value off of a list in SPSA
peel :: (SPSA -> [a]) -> ([a] -> SPSA -> SPSA) -> StateSPSA a
peel getter updater = do
  spsa <- get
  let sq = getter spsa
  let ([nxt],rst) = splitAt 1 sq
  put $ updater rst spsa
  return nxt

-- | Peel a value off of the gain sequence a_k
peelA :: StateSPSA Double
peelA = peel gainA (\as spsa -> spsa { gainA = as })

-- | Peel a value off of the gain sequence a_k
peelC :: StateSPSA Double
peelC = peel gainC (\cs spsa -> spsa { gainC = cs })

-- | Peel a value off of the perturbation vector sequence
peelD :: StateSPSA (Vector Double)
peelD = peel perturbation (\ds spsa -> spsa { perturbation = ds })

-- | Peel the next value from the two gain sequences and the perturbation vectors
peelAll :: StateSPSA (Double, Double, Vector Double)
peelAll = do
  a <- peelA
  c <- peelC
  d <- peelD
  return (a, c, d)

-- | Set the gainA sequence
setGainA :: [Double] -> StateSPSA ()
setGainA = setSPSA $ \as spsa -> spsa { gainA = as }

-- | Set the gainC sequence
setGainC :: [Double] -> StateSPSA ()
setGainC = setSPSA $ \cs spsa -> spsa { gainC = cs }

-- | Set the perturbation sequence
setPerturbation :: [Vector Double] -> StateSPSA ()
setPerturbation = setSPSA $ \ds spsa -> spsa { perturbation = ds }