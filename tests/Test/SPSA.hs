module Test.SPSA where

import Test.Framework (defaultMainWithArgs, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.HUnit (assertBool)

import Data.Word (Word16, Word32)
import Data.List (genericIndex, genericTake)
import System.Random (Random(..))

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

props_standardAkValues (UDouble a) (UInt aA) (Exponent alpha) (SmallIndex n) =
  ak !! n == (a / ((fromIntegral n) + 2 + (fromIntegral aA)) ** alpha)
  where ak = standardAk (UDouble a) (UInt aA) (Exponent alpha)

props_standardAkDecreasing :: UDouble -> UInt -> Exponent -> SmallIndex -> Bool
props_standardAkDecreasing a aA alpha (SmallIndex n) =
  monotonicallyDecreasing (take n (standardAk a aA alpha))

props_standardCkValues (UDouble c) (Exponent gamma) (SmallIndex n) =
  ck !! n == (c / ((fromIntegral n) + 2) ** gamma)
  where ck = standardCk (UDouble c) (Exponent gamma)

props_standardCkDecreasing :: UDouble -> Exponent -> SmallIndex -> Bool
props_standardCkDecreasing c gamma (SmallIndex n) =
  monotonicallyDecreasing (take n (standardCk c gamma))

props_bernoulliLength :: SmallIndex -> SmallIndex -> Property
props_bernoulliLength (SmallIndex n) (SmallIndex m) =
    monadicIO test
  where
    test = do pd <- run $ bernoulli (UInt n); assert $ length (pd !! m) == n

props_bernoulliElements :: SmallIndex -> SmallIndex -> Property
props_bernoulliElements (SmallIndex n) (SmallIndex m) = monadicIO test
  where test = do pd <- run $ bernoulli (UInt n); assert $ all (\e -> e == -1 || e == 1) (pd !! m)

props_semiautomaticTuningAk :: UDouble -> SmallIndex -> Bool
props_semiautomaticTuningAk a (SmallIndex n) = all (\(a1,a2) -> a1 == a2) (zip ak (take n tak))
  where (tak,_) = semiautomaticTuning a 1
        ak = standardAk a 0 0.602

props_semiautomaticTuningCk :: UDouble -> SmallIndex -> Bool
props_semiautomaticTuningCk c (SmallIndex n) = all (\(a1,a2) -> a1 == a2) (zip ck (take n tck))
  where (_,tck) = semiautomaticTuning 1 c
        ck = standardCk c 0.101

case_absSumSPSA = do
  (ak,ck) <- return $ semiautomaticTuning 1 0.1
  spsa <- mkUnconstrainedSPSA absSum ak ck (UInt 5)
  output <- return $ optimize spsa (UInt 1000) [1, 1, 1, 1, 1]
  assertBool ("SPSA Absolute Sum failed (" ++ (show $ absSum output) ++ ")") (absSum output < 0.001)

case_rosenbrockSPSA = do
  (ak,ck) <- return $ semiautomaticTuning 0.002 0.05
  spsa <- mkUnconstrainedSPSA rosenbrock ak ck (UInt 10)
  output <- return $ optimize spsa (UInt 10000) [0.99,1,0.99,1,0.99,1,0.99,1,0.99,1]
  assertBool ("SPSA Rosenbrock failed (" ++ (show $ rosenbrock output) ++ ")") (rosenbrock output < 0.001)

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
  ,testGroup "SPSA" [
    testCase "absSum SPSA" case_absSumSPSA
    ,testCase "rosenbrock SPSA" case_rosenbrockSPSA
    ]
  ]

-- convenient whilst in ghci
runAllTests = defaultMainWithArgs tests []
runTests p = defaultMainWithArgs tests ["--select-tests", p]
runGroup i = defaultMainWithArgs [tests !! i] []