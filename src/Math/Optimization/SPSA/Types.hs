module Math.Optimization.SPSA.Types (
  Exponent(..),
  UDouble(..),
  UInt(..),
  SPSA(..)
) where

import System.Random (Random)

-- An instance of the SPSA optimization algorithm.
-- Initialize with all the parameters as object instantiation.
data SPSA = SPSA {
  loss :: [Double] -> Double,
  constraint :: [Double] -> [Double],
  ak, ck :: [Double],
  delta :: [[Double]] -- a stream of random perturbation vectors
}


newtype Exponent = Exponent Double deriving (Read,Show,Eq,Ord,Num,Fractional,Floating,Real,RealFloat,RealFrac,Enum,Random)
instance Bounded Exponent where
  minBound = 0.1
  maxBound = 1

newtype UDouble = UDouble Double deriving (Read,Show,Eq,Ord,Num,Fractional,Floating,Real,RealFloat,RealFrac,Enum,Random)
instance Bounded UDouble where
  minBound = 0
  maxBound = UDouble (maxNonInfiniteFloat 1)

newtype UInt = UInt Int deriving (Read,Show,Eq,Ord,Num,Real,Enum,Integral)
instance Bounded UInt where
  minBound = 0
  maxBound = UInt (maxBound :: Int)

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