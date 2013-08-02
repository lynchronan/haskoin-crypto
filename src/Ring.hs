{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module Ring
( Hash256
, Hash160
, FieldP
, FieldN
, curveN
, curveP
, Ring(..)
, RingMod(..)
, toFieldN
, toFieldP
, toMod256
, toMod160
, inverseP
, inverseN
) where

import Data.Bits
import Data.Word
import Data.Binary
import Data.Binary.Get 
import Data.Binary.Put
import Control.Applicative ((<$>))
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

toFieldN :: Ring n -> FieldN
toFieldN (Ring i) = fromInteger i

toFieldP :: Ring n -> FieldP
toFieldP (Ring i) = fromInteger i

toMod256 :: Ring n -> Hash256
toMod256 (Ring i) = fromInteger i

toMod160 :: Ring n -> Hash160
toMod160 (Ring i) = fromInteger i

inverseP :: FieldP -> FieldP
inverseP (Ring i) = fromInteger $ mulInverse i curveP

inverseN :: FieldN -> FieldN
inverseN (Ring i) = fromInteger $ mulInverse i curveN

class RingMod a where 
    rFromInteger :: Integer -> Ring a
    rBitSize     :: Ring a -> Int

instance RingMod Mod256 where
    rFromInteger i = Ring $ i `mod` 2^256
    rBitSize     _ = 256

instance RingMod Mod160 where
    rFromInteger i = Ring $ i `mod` 2^160
    rBitSize     _ = 160

instance RingMod ModP where
    rFromInteger i = Ring $ i `mod` curveP
    rBitSize     _ = 256

instance RingMod ModN where
    rFromInteger i = Ring $ i `mod` curveN
    rBitSize     _ = 256

instance RingMod n => Num (Ring n) where
    fromInteger = rFromInteger
    (Ring i1) + (Ring i2) = fromInteger $ i1 + i2
    (Ring i1) * (Ring i2) = fromInteger $ i1 * i2
    negate (Ring i) = fromInteger $ negate i
    abs r = r
    signum (Ring i) = fromInteger $ signum i

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

{- Fractional is only defined for prime orders -}

instance Fractional (Ring ModP) where
    recip = inverseP
    fromRational r = (fromInteger (numerator r)) / (fromInteger (denominator r))

instance Fractional (Ring ModN) where
    recip = inverseN
    fromRational r = (fromInteger (numerator r)) / (fromInteger (denominator r))

{- Binary instances for serialization / deserialization -}

instance Binary (Ring Mod256) where
    get = do
        a <- fromIntegral <$> getWord64be
        b <- fromIntegral <$> getWord64be
        c <- fromIntegral <$> getWord64be
        d <- fromIntegral <$> getWord64be
        return $ fromInteger $
            (a `shiftL` 192) + (b `shiftL` 128) + (c `shiftL` 64) + d

    put (Ring i) = do
        putWord64be $ fromIntegral (i `shiftR` 192)
        putWord64be $ fromIntegral (i `shiftR` 128)
        putWord64be $ fromIntegral (i `shiftR` 64)
        putWord64be $ fromIntegral i

instance Binary (Ring Mod160) where
    get = do
        a <- fromIntegral <$> getWord32be
        b <- fromIntegral <$> getWord64be
        c <- fromIntegral <$> getWord64be
        return $ fromInteger $ (a `shiftL` 128) + (b `shiftL` 64) + c

    put (Ring i) = do
        putWord32be $ fromIntegral (i `shiftR` 128)
        putWord64be $ fromIntegral (i `shiftR` 64)
        putWord64be $ fromIntegral i


