module Math.Optimization.SPSA (
  -- Package spsa provides the Simultaneous Perturbation Stochastic Approximation method.
  --
  -- Much of the notation is taken from Introduction To Stochastic Search and Optimization
  -- (ISSO), James Spall's book on Stochastic Optimization. (published by Wiley 2003)
  --

  module Math.Optimization.SPSA.Types,
  module Math.Optimization.SPSA.Optimize,
  module Math.Optimization.SPSA.Construction
) where

import Math.Optimization.SPSA.Types (
  StateSPSA, defaultSPSA,
  setLoss, setConstraint, pushStopCrit, setGainA, setGainC, setPerturbation,
  StoppingCriteria(..),
  LossFn,
  ConstraintFn)
import Math.Optimization.SPSA.Optimize
import Math.Optimization.SPSA.Construction