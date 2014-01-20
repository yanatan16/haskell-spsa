module Test.Test where

import Test.Framework (defaultMainWithArgs, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (NonNegative(..), (==>))
import Test.HUnit (assertBool, assertFailure)

import Control.Exception (try, ErrorCall(..))

import qualified Numeric.LinearAlgebra as LA
import Math.Optimization.SPSA
import Math.Optimization.LossFunctions (absSum, rosenbrock)
import Test.Types
import System.Random (randomIO)

monotonicallyNonincreasing :: Ord a => [a] -> Bool
monotonicallyNonincreasing (x:y:xs) = x >= y && monotonicallyNonincreasing (y:xs)
monotonicallyNonincreasing _ = True

assertErrorCall msg action = do
  r <- try action
  case r of
    Left (ErrorCall _) -> return ()
    --Left e -> assertFailure $ msg ++ "\nReceived an unexpected exception: " ++ (show e)
    Right _ -> assertFailure $ msg ++ "\nReceived no exception, but was expecting an error"


-----------------
-- Properties
-----------------

props_standardGainAkValues (NonNegativeDouble a) (NonNegative aA) (Exponent alpha) (SmallIndex n) =
  gain !! n == (a / ((fromIntegral n) + 2 + (fromIntegral aA)) ** alpha)
  where gain = standardGainAk a aA alpha

props_standardGainAkNonincreasing (NonNegative a) (NonNegative aA) (Exponent alpha) (SmallIndex n) =
  monotonicallyNonincreasing (take n (standardGainAk a aA alpha))

props_standardGainCkValues (NonNegativeDouble c) (Exponent gamma) (SmallIndex n) =
  gain !! n == (c / ((fromIntegral n) + 2) ** gamma)
  where gain = standardGainCk c gamma

props_standardGainCkNonincreasing (NonNegativeDouble c) (Exponent gamma) (SmallIndex n) =
  monotonicallyNonincreasing (take n (standardGainCk c gamma))

props_bernoulliLength seed (SmallIndex n) = n >= 0 ==> all (\v -> LA.dim v == n) $ take 10 $ bernoulli seed n

props_bernoulliElements seed (SmallIndex n) = all (all (== 1) . LA.toList . abs) $ take 10 $ bernoulli seed n

props_lossFunctionPositive lss xs@(_:_:_) = ((>= 0.0) . lss . LA.fromList) xs
props_lossFunctionPositive _ [] = True
props_lossFunctionPositive _ [_] = True

mkAbsSumSPSA seed = do
  setLoss absSum
  semiautomaticTuning 1000 1 0.1
  setPerturbation (bernoulli seed 5)
  pushStopCrit (Iterations 1000)

case_absSumSPSA = do
  seed <- randomIO
  let output = runSPSA (mkAbsSumSPSA seed) (LA.fromList [1, 1, 1, 1, 1])
  assertBool ("SPSA Absolute Sum failed (" ++ (show $ absSum output) ++ ")") (absSum output < 0.001)

mkRosenbrockSPSA seed = do
  setLoss rosenbrock
  semiautomaticTuning 10000 0.001 0.05
  setPerturbation (bernoulli seed 10)
  pushStopCrit (Iterations 10000)

case_rosenbrockSPSA = do
  seed <- randomIO
  let output = runSPSA (mkRosenbrockSPSA seed) (LA.fromList [0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1,0.90,1.1])
  assertBool ("SPSA Rosenbrock failed (" ++ (show $ rosenbrock output) ++ ")") (rosenbrock output < 0.01)

case_unconfiguredSPSA = do
  assertErrorCall "SPSA should error if used without configuration" (print $ runSPSA (return ()) (LA.fromList [1]))

tests = [
  testGroup "standardGainAk" [
    testProperty "nth term of standardGainAk" props_standardGainAkValues
    ,testProperty "monotonially Nonincreasing standardGainAk" props_standardGainAkNonincreasing
    ]
  ,testGroup "standardGainCk" [
    testProperty "nth term of standardGainCk" props_standardGainCkValues
    ,testProperty "monotonially Nonincreasing standardGainCk" props_standardGainCkNonincreasing
    ]
  ,testGroup "bernoulliPerturbationDistribution" [
    testProperty "length of perturbation distribution" props_bernoulliLength
    ,testProperty "elements are bernoulli in perturbation vector" props_bernoulliElements
    ]
  ,testGroup "lossFunctions" [
    testProperty "absSum" (props_lossFunctionPositive absSum)
    ,testProperty "rosenbrock" (props_lossFunctionPositive rosenbrock)
    ]
  ,testGroup "Configuring SPSA" [
    testCase "error on running uninitialized SPSA" case_unconfiguredSPSA
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