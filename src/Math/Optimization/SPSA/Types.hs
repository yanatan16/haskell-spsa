module Math.Optimization.SPSA.Types (
  SPSA(..),
  LossFn, ConstraintFn
) where

import Numeric.LinearAlgebra (Vector)

type LossFn = Vector Double -> Double
type ConstraintFn = Vector Double -> Vector Double

-- An instance of the SPSA optimization algorithm.
-- Initialize with all the parameters as object instantiation.
data SPSA = SPSA {
  loss :: LossFn,
  constraint :: ConstraintFn,
  ak, ck :: [Double],
  delta :: [Vector Double] -- a stream of random perturbation vectors
}