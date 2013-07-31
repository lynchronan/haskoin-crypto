module Ring
( Hash256
, Hash160
, FieldP
, FieldN
, curveN
, curveP
, Ring
, RingMod
) where

import Data.Bits
import Data.Ratio (numerator, denominator)
import NumberTheory (mulInverse)

type Hash256 = Ring Mod256
type Hash160 = Ring Mod160
type FieldP  = Ring ModP
type FieldN  = Ring ModN

data Mod256 
data Mod160
data ModP
data ModN

curveP :: Integer
curveP = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f 

curveN :: Integer
curveN = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 

newtype Ring n = Ring { runRing :: Integer }
    deriving (Show, Eq, Ord)

class RingMod a where 
    rFromInteger :: Integer -> Ring a
    rMod         :: Ring a -> Integer
    rBitSize     :: Ring a -> Int

instance RingMod Mod256 where
    rFromInteger i = Ring $ i `mod` 2^256
    rMod         _ = 2^256
    rBitSize     _ = 256

instance RingMod Mod160 where
    rFromInteger i = Ring $ i `mod` 2^160
    rMod         _ = 2^160
    rBitSize     _ = 160

instance RingMod ModP where
    rFromInteger i = Ring $ i `mod` curveP
    rMod         _ = curveP
    rBitSize     _ = 256

instance RingMod ModN where
    rFromInteger i = Ring $ i `mod` curveN
    rMod         _ = curveN
    rBitSize     _ = 256

instance RingMod n => Num (Ring n) where
    fromInteger = rFromInteger
    (Ring i1) + (Ring i2) = fromInteger $ i1 + i2
    (Ring i1) * (Ring i2) = fromInteger $ i1 * i2
    negate (Ring i) = fromInteger $ negate i
    abs r = r
    signum _ = 1

instance RingMod n => Bits (Ring n) where
    (Ring i1) .&. (Ring i2) = fromInteger $ i1 .&. i2
    (Ring i1) .|. (Ring i2) = fromInteger $ i1 .|. i2
    (Ring i1) `xor` (Ring i2) = fromInteger $ i1 `xor` i2
    complement (Ring i) = fromInteger $ complement i
    shift (Ring i) j = fromInteger $ shift i j
    bitSize = rBitSize
    testBit (Ring i) = testBit i
    bit n = fromInteger $ bit n
    popCount (Ring i) = popCount i
    isSigned _ = False

instance RingMod n => Fractional (Ring n) where
    recip a@(Ring i) = fromInteger $ mulInverse i (rMod a)
    fromRational r = (fromInteger (numerator r)) / (fromInteger (denominator r))


