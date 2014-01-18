module Main where

import Criterion.Main (defaultMain, bench, bgroup, Benchmark)

import qualified Numeric.LinearAlgebra as LA

import Math.Optimization.SPSA (mkUnconstrainedSPSA, semiautomaticTuning, optimize)
import Math.Optimization.LossFunctions

import System.Random (randomIO)

---------------------
-- Benchmarks
---------------------

bench_rosenbrock :: Int -> IO ()
bench_rosenbrock n = do
  seed <- randomIO
  let (ak,ck) = semiautomaticTuning n 0.0001 0.05
  let spsa = mkUnconstrainedSPSA seed rosenbrock ak ck 10
  let output = optimize spsa n (LA.fromList [0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1])
  if rosenbrock output > 1 then error "Failed Rosenbrock!" else return ()

bench_absSum :: Int -> Int -> IO ()
bench_absSum d n = do
  seed <- randomIO
  let (ak,ck) = semiautomaticTuning n 1 0.1
  let spsa = mkUnconstrainedSPSA seed absSum ak ck d
  let output = optimize spsa n (LA.constant 1 d)
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