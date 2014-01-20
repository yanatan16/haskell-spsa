module Main where

import Criterion.Main (defaultMain, bench, bgroup, Benchmark)

import qualified Numeric.LinearAlgebra as LA

import Math.Optimization.SPSA (
  StateSPSA, runSPSA,
  setLoss,
  semiautomaticTuning,
  setPerturbation, bernoulli,
  pushStopCrit, StoppingCriteria(..)
  )
import Math.Optimization.LossFunctions (absSum, rosenbrock)

import System.Random (randomIO)

---------------------
-- Benchmarks
---------------------

mkRosenbrockSPSA seed n d = do
  setLoss rosenbrock
  semiautomaticTuning n 0.0001 0.05
  setPerturbation (bernoulli seed d)
  pushStopCrit (Iterations n)
  --pushStopCrit (NormDiff 0.001)

bench_rosenbrock n = do
  seed <- randomIO
  let spsa = mkRosenbrockSPSA seed n 10
  let theta0 = LA.fromList [0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1]
  let output = runSPSA spsa theta0
  if rosenbrock output > 1 then error ("Failed Rosenbrock! " ++ (show $ rosenbrock output) ++ ": " ++ (show output)) else return ()

mkAbsSumSPSA seed n d = do
  setLoss absSum
  semiautomaticTuning n 1 0.1
  setPerturbation (bernoulli seed d)
  pushStopCrit (Iterations n)

bench_absSum d n = do
  seed <- randomIO
  let spsa = mkAbsSumSPSA seed n d
  let theta0 = (LA.constant 1 d)
  let output = runSPSA spsa theta0
  if absSum output > 1 then error "Failed Absolute Sum!" else return ()

---------------------
-- List of Benchmarks
---------------------

benches :: [Benchmark]
benches = [
  bgroup "Rosenbrock" [
    bench "n=10000" (bench_rosenbrock 10000)
    ]
  ,bgroup "AbsoluteSum" [
    bench "n=5 1000 iterations" (bench_absSum 5 1000)
    ]
  ]


---------------------
-- Main
---------------------

main :: IO ()
main = defaultMain benches