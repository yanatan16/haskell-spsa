module Math.Optimization.SPSA.Optimize (
  runSPSA
) where

import Control.Monad.State (evalState)
import Numeric.LinearAlgebra (Vector,scale,scaleRecip)

import Math.Optimization.SPSA.Types (
  StateSPSA, defaultSPSA, checkSPSA,
  getLoss, getConstraint,
  peelAll,
  getStop, shouldStop,
  getIterations, incrementIteration
  )

-------------------
-- Optimize SPSA --
-------------------

-- | Exported runSPSA function to extract the SPSA type
runSPSA :: StateSPSA a -> Vector Double -> Vector Double
runSPSA st t0 = evalState (st >> checkSPSA t0 >> runSPSA' t0) defaultSPSA

-- | Run the SPSA optimization algorithm
runSPSA' :: Vector Double -> StateSPSA (Vector Double)
runSPSA' t = do
  t' <- singleIteration t
  stop <- checkStop t t'
  incrementIteration
  if stop then return t' else runSPSA' t'

-- | Perform a single iteration of SPSA
singleIteration :: Vector Double -> StateSPSA (Vector Double)
singleIteration t = do
  (a, c, d) <- peelAll
  lossF <- getLoss
  constrainF <- getConstraint
  let cd = c `scale` d
  let ya = lossF (t + cd)
  let yb = lossF (t - cd)
  let grad = ((ya - yb) / 2) `scaleRecip` cd
  return $ constrainF (t - (a `scale` grad))

-- | Check the stopping criteria to see if we should stop
checkStop :: Vector Double -> Vector Double -> StateSPSA Bool
checkStop t t' = do
  crits <- getStop
  iter <- getIterations
  return $ any (\c -> shouldStop c iter t t') crits