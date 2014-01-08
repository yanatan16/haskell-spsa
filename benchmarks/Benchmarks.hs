module Main where

import Criterion.Main (defaultMain, bench, bgroup, whnf)

import Math.Optimization.SPSA (mkUnconstrainedSPSA, semiautomaticTuning, optimize, UInt(..))
import Math.Optimization.LossFunctions

---------------------
-- Benchmarks
---------------------

bench_rosenbrock n = do
  (ak,ck) <- return $ semiautomaticTuning 0.0001 0.05
  spsa <- mkUnconstrainedSPSA rosenbrock ak ck (UInt 10)
  output <- return $ optimize spsa (UInt n) [0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1]
  if rosenbrock output > 1 then error "Failed Rosenbrock!" else return ()

bench_absSum d n = do
  (ak,ck) <- return $ semiautomaticTuning 1 0.1
  spsa <- mkUnconstrainedSPSA rosenbrock ak ck (UInt d)
  return $ optimize spsa (UInt n) (take d $ repeat 1)

---------------------
-- List of Benchmarks
---------------------

benches = [
  bgroup "Rosenbrock" [
    bench "n=10000 iterations" (bench_rosenbrock 10000)
    ]
  --,bgroup "AbsoluteSum" [
  --  bench "n=100 100 iterations" (bench_absSum 100 100)
  --  ,bench "n=100 1000 iterations" (bench_absSum 100 1000)
  --  ,bench "n=100 10000 iterations" (bench_absSum 100 10000)
  --  ,bench "n=200 100 iterations" (bench_absSum 200 100)
  --  ,bench "n=200 1000 iterations" (bench_absSum 200 1000)
  --  ,bench "n=200 10000 iterations" (bench_absSum 200 10000)
  --  ]
  ]


---------------------
-- Main
---------------------

main = defaultMain benches