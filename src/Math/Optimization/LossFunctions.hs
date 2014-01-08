module Math.Optimization.LossFunctions (
  absSum,
  rosenbrock
) where

import Numeric.LinearAlgebra (Vector(..),dim,subVector,foldVector,toList)
import Math.Optimization.SPSA.Types (LossFn)

absSum :: LossFn
absSum = sumVector . abs

sumVector :: Vector Double -> Double
sumVector = foldVector (+) 0

rosenbrock :: LossFn
rosenbrock v = sumVector $ 100 * (x ^ 2 - y) ^2 + (x - 1) ^ 2
  where
    sz = (dim v) `quot` 2
    x = subVector 0 sz v
    y = subVector sz sz v

pairs :: [a] -> [(a,a)]
pairs xs = pairs' xs []

pairs' :: [a] -> [(a,a)] -> [(a,a)]
pairs' [] acc = acc
pairs' [x] acc = acc
pairs' (x:y:xs) acc = pairs' xs ((x,y) : acc)