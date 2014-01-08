module Test.Test where

import Test.Framework (defaultMainWithArgs, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, NonNegative(..))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.HUnit (assertBool)

import qualified Numeric.LinearAlgebra as LA
import Math.Optimization.SPSA
import Math.Optimization.LossFunctions (absSum, rosenbrock)
import Test.Types

monotonicallyDecreasing :: [Double] -> Bool
monotonicallyDecreasing xs = fst $ foldl ensureDecreasing (True, 0) xs
  where
    ensureDecreasing (False,_) _ = (False,0)
    ensureDecreasing (True,0) x = (True,x)
    ensureDecreasing (True,x) y = (x >= y, y)

-----------------
-- Properties
-----------------

props_standardAkValues :: NonNegativeDouble -> NonNegative Int -> Exponent Double -> SmallIndex -> Bool
props_standardAkValues (NonNegativeDouble a) (NonNegative aA) (Exponent alpha) (SmallIndex n) =
  gain !! n == (a / ((fromIntegral n) + 2 + (fromIntegral aA)) ** alpha)
  where gain = standardAk a aA alpha

props_standardAkDecreasing :: NonNegative Double -> NonNegative Int -> Exponent Double -> SmallIndex -> Bool
props_standardAkDecreasing (NonNegative a) (NonNegative aA) (Exponent alpha) (SmallIndex n) =
  monotonicallyDecreasing (take n (standardAk a aA alpha))

props_standardCkValues :: NonNegativeDouble -> Exponent Double -> SmallIndex -> Bool
props_standardCkValues (NonNegativeDouble c) (Exponent gamma) (SmallIndex n) =
  gain !! n == (c / ((fromIntegral n) + 2) ** gamma)
  where gain = standardCk c gamma

props_standardCkDecreasing :: NonNegativeDouble -> Exponent Double -> SmallIndex -> Bool
props_standardCkDecreasing (NonNegativeDouble c) (Exponent gamma) (SmallIndex n) =
  monotonicallyDecreasing (take n (standardCk c gamma))

props_bernoulliLength :: SmallIndex -> Property
props_bernoulliLength (SmallIndex n) =
    monadicIO test
  where
    test = do pd <- run $ bernoulli n
              assert $ all (\v -> LA.dim v == n) (take 10 pd)

props_bernoulliElements :: SmallIndex -> Property
props_bernoulliElements (SmallIndex n) = monadicIO test
  where test = do pd <- run $ bernoulli n
                  assert $ all (\v -> all (== 1) $ LA.toList $ abs v) (take 10 pd)

props_semiautomaticTuningAk :: NonNegativeDouble -> SmallIndex -> Bool
props_semiautomaticTuningAk (NonNegativeDouble a) (SmallIndex n) = all (\(a1,a2) -> a1 == a2) (zip gain (take n tak))
  where (tak,_) = semiautomaticTuning a 1
        gain = standardAk a 0 0.602

props_semiautomaticTuningCk :: NonNegativeDouble -> SmallIndex -> Bool
props_semiautomaticTuningCk (NonNegativeDouble c) (SmallIndex n) = all (\(a1,a2) -> a1 == a2) (zip gain (take n tck))
  where (_,tck) = semiautomaticTuning 1 c
        gain = standardCk c 0.101

props_lossFunctionPositive :: LossFn -> [Double] -> Bool
props_lossFunctionPositive _ [] = True
props_lossFunctionPositive _ [_] = True
props_lossFunctionPositive lss xs = ((>= 0.0) . lss . LA.fromList) xs

case_absSumSPSA :: IO ()
case_absSumSPSA = do
  (gainA,gainC) <- return $ semiautomaticTuning 1 0.1
  spsa <- mkUnconstrainedSPSA absSum gainA gainC 5
  output <- return $ optimize spsa 1000 (LA.fromList [1, 1, 1, 1, 1])
  assertBool ("SPSA Absolute Sum failed (" ++ (show $ absSum output) ++ ")") (absSum output < 0.001)

case_rosenbrockSPSA :: IO ()
case_rosenbrockSPSA = do
  (gainA,gainC) <- return $ semiautomaticTuning 0.0001 0.05
  spsa <- mkUnconstrainedSPSA rosenbrock gainA gainC 10
  output <- return $ optimize spsa 10000 (LA.fromList [0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1])
  assertBool ("SPSA Rosenbrock failed (" ++ (show $ rosenbrock output) ++ ")") (rosenbrock output < 0.01)

tests :: [Test]
tests = [
  testGroup "standardAk" [
    testProperty "nth term of standardAk" props_standardAkValues
    ,testProperty "monotonially decreasing standardAk" props_standardAkDecreasing
    ]
  ,testGroup "standardCk" [
    testProperty "nth term of standardCk" props_standardCkValues
    ,testProperty "monotonially decreasing standardCk" props_standardCkDecreasing
    ]
  ,testGroup "semiautomaticTuning" [
    testProperty "ak matches" props_semiautomaticTuningAk
    ,testProperty "ck matches" props_semiautomaticTuningCk
    ]
  ,testGroup "bernoulliPerturbationDistribution" [
    testProperty "length of perturbation distribution" props_bernoulliLength
    ,testProperty "elements are bernoulli in perturbation vector" props_bernoulliElements
    ]
  ,testGroup "lossFunctions" [
    testProperty "absSum" (props_lossFunctionPositive absSum)
    ,testProperty "rosenbrock" (props_lossFunctionPositive rosenbrock)
    ]
  ,testGroup "SPSA" [
    testCase "absSum SPSA" case_absSumSPSA
    ,testCase "rosenbrock SPSA" case_rosenbrockSPSA
    ]
  ]

-- convenient whilst in ghci
runAllTests :: IO ()
runAllTests = defaultMainWithArgs tests []

runTests :: String -> IO ()
runTests p = defaultMainWithArgs tests ["--select-tests", p]

runGroup :: Int -> IO ()
runGroup i = defaultMainWithArgs [tests !! i] []