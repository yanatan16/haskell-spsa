module Test.Types (
  SmallIndex(..),
  SmallDouble(..),
  Exponent(..),
  NonNegativeDouble(..)
) where

import System.Random (Random)

import Test.QuickCheck (Arbitrary(..),
        arbitraryBoundedRandom, arbitraryBoundedIntegral,
        shrinkIntegral, shrinkRealFrac,)

newtype Exponent d = Exponent d deriving (Read,Show,Eq,Ord,Num,Real,Enum,Floating,Fractional,RealFrac,RealFloat,Random)
instance (Num d, RealFrac d) => Bounded (Exponent d) where
  minBound = 0.000000001
  maxBound = 1

instance (Num d, RealFrac d, Random d) => Arbitrary (Exponent d) where
  arbitrary = arbitraryBoundedRandom
  shrink = shrinkRealFrac

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

newtype NonNegativeDouble = NonNegativeDouble Double deriving (Read,Show,Eq,Ord,Num,Real,Enum,RealFrac,Floating,RealFloat,Fractional,Random)

instance Bounded NonNegativeDouble where
  minBound = 0
  maxBound = maxNonInfiniteFloat 0

instance Arbitrary NonNegativeDouble where
  arbitrary = arbitraryBoundedRandom
  shrink = shrinkRealFrac

-----------------
-- Helpers
-----------------

maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e