module Math.Optimization.LossFunctions (
  absSum,
  rosenbrock
) where

import Numeric.LinearAlgebra (Vector,dim,subVector,foldVector,mul)
import Math.Optimization.SPSA.Types (LossFn)

absSum :: LossFn
absSum = sumVector . abs

sumVector :: Vector Double -> Double
sumVector = foldVector (+) 0

square :: Vector Double -> Vector Double
square x = mul x x

rosenbrock :: LossFn
rosenbrock v = sumVector $ 100 * (square $ (square x) - y) + (square (x - 1))
  where
    sz = (dim v) `quot` 2
    x = subVector 0 sz v
    y = subVector sz sz v
