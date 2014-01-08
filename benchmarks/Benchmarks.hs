module Main where

import Criterion.Main (defaultMain, bench, bgroup, Benchmark)

import qualified Numeric.LinearAlgebra as LA

import Math.Optimization.SPSA (mkUnconstrainedSPSA, semiautomaticTuning, optimize)
import Math.Optimization.LossFunctions

---------------------
-- Benchmarks
---------------------

bench_rosenbrock :: Int -> IO ()
bench_rosenbrock n = do
  (ak,ck) <- return $ semiautomaticTuning 0.0001 0.05
  spsa <- mkUnconstrainedSPSA rosenbrock ak ck 10
  output <- return $ optimize spsa n (LA.fromList [0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1])
  if rosenbrock output > 1 then error "Failed Rosenbrock!" else return ()

bench_absSum :: Int -> Int -> IO ()
bench_absSum d n = do
  (ak,ck) <- return $ semiautomaticTuning 1 0.1
  spsa <- mkUnconstrainedSPSA absSum ak ck d
  output <- return $ optimize spsa n (LA.constant 1 d)
  if absSum output > 1 then error "Failed Absolute Sum!" else return ()

---------------------
-- List of Benchmarks
---------------------

benches :: [Benchmark]
benches = [
  bgroup "Rosenbrock" [
    bench "n=10000" (bench_rosenbrock 10000)
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

main :: IO ()
main = defaultMain benches