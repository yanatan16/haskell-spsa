module Math.Optimization.LossFunctions (
  absSum,
  rosenbrock
) where

absSum :: Num n => [n] -> n
absSum = sum . map abs

rosenbrock :: Floating n => [n] -> n
rosenbrock v = sum $ map inner $ zip [0..] v
  where
    inner (i,x) = if odd i
      then
        0
      else
        x + 100 * ((v !! i) ^ 2) - (v !! (i+1)) ^ 2 + ((v !! i) - 1) ^ 2