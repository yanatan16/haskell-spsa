module Main where

import System.Random (randomIO)
import System.Environment (getArgs)
import Numeric.LinearAlgebra (Vector, fromList, randomVector, RandDist(Uniform))
import Numeric.Container (Container (mul), sumElements)
import Math.Optimization.SPSA (
  StateSPSA, runSPSA,
  setLoss,
  setPerturbation, bernoulli,
  semiautomaticTuning,
  pushStopCrit, StoppingCriteria(..))

sphereLossFn :: Vector Double -> Double
sphereLossFn vd = sumElements (vd `mul` vd)

configSPSA :: Int -> Int -> Int -> StateSPSA ()
configSPSA seed dim iter = do
  setLoss sphereLossFn
  setPerturbation (bernoulli seed dim)
  pushStopCrit (Iterations iter)
  semiautomaticTuning iter 0.1 0.1

initGuess :: Int -> Int -> Vector Double
initGuess seed = randomVector seed Uniform

main = do
  seed1 <- randomIO
  seed2 <- randomIO
  args <- getArgs
  let (dim, iter) = case args of  [] -> (10, 100)
                                  [d] -> (read d, (read d) * 10)
                                  (d:i:_) -> (read d, read i)
  putStrLn $ "Running Sphere Loss Function with dimension " ++ (show dim) ++ " and iterations " ++ (show iter)
  let t0 = initGuess seed1 dim
  putStrLn $ "Initial guess loss " ++ (show $ sphereLossFn t0) ++ " (" ++ (show t0) ++ ")"
  let spsa = configSPSA seed2 dim iter
  let tfinal = runSPSA spsa t0
  putStrLn $ "Ran SPSA: final guess loss " ++ (show $ sphereLossFn tfinal) ++ " (" ++ (show tfinal) ++ ")"
