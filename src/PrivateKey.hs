module PrivateKey
( PrivateKey
, runFn
) where

import Data.Ratio (numerator, denominator)
import NumberTheory (extendedModGCD, mulInverse)

type PrivateKey = Fn

n :: Integer
n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 

newtype Fn = Fn { runFn :: Integer }
    deriving (Eq, Show)

--Point coordinates are stored modulo p
instance Num Fn where
    fromInteger a = Fn $ a `mod` n
    (Fn a) + (Fn b) = fromInteger $ a + b
    negate (Fn a) = fromInteger $ negate a
    (Fn a) * (Fn b) = fromInteger $ a * b
    abs _ = error "abs is not defined for EC point coordinates"
    signum _ = error "signum is not defined for EC point coordinates"

-- a divides b if there exists c such that b = ac (mod p)
instance Fractional Fn where
    recip (Fn a) = fromInteger $ mulInverse a n
    fromRational r = (fromInteger (numerator r)) / (fromInteger (denominator r))

