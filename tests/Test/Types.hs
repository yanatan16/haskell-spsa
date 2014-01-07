module Test.Types (
  SmallIndex(..),
  SmallDouble(..), toDouble,
  Exponent(..),
  UDouble(..),
  UInt(..)
) where

import Math.Optimization.SPSA (Exponent(..), UDouble(..), UInt(..))
import System.Random (Random)

import Test.QuickCheck (Arbitrary(..),
        arbitraryBoundedRandom, arbitraryBoundedIntegral, arbitrarySizedBoundedIntegral,
        shrinkIntegral, shrinkRealFrac,)

instance Arbitrary Exponent where
  arbitrary = arbitraryBoundedRandom
  shrink = shrinkRealFrac

instance Arbitrary UDouble where
  arbitrary = arbitraryBoundedRandom
  shrink = shrinkRealFrac

instance Arbitrary UInt where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = shrinkIntegral

newtype SmallIndex = SmallIndex Int deriving (Read,Show,Eq,Ord,Num,Real,Enum,Integral)

instance Bounded SmallIndex where
  minBound = 0
  maxBound = 1000

instance Arbitrary SmallIndex where
  arbitrary = arbitraryBoundedIntegral
  shrink = shrinkIntegral

newtype SmallDouble = SmallDouble Double deriving (Read,Show,Eq,Ord,Num,Real,Enum,RealFrac,Fractional,Random)

instance Bounded SmallDouble where
  minBound = -1000
  maxBound = 1000

instance Arbitrary SmallDouble where
  arbitrary = arbitraryBoundedRandom
  shrink = shrinkRealFrac

toDouble (SmallDouble d) = d