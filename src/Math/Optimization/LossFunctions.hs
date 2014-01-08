module Math.Optimization.LossFunctions (
  absSum,
  rosenbrock
) where

absSum :: Num n => [n] -> n
absSum = sum . map abs

rosenbrock :: RealFloat n => [n] -> n
rosenbrock v = sum $ map inner $ zip [0..] v
  where
    inner (i,x) = if (i + 1 == length v) || (odd i)
      then
        0
      else
        100 * ((x ^ 2) - (v !! (i+1))) ^ 2 + (x - 1) ^ 2