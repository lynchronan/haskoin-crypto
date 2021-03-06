{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module Haskoin.Crypto.Ring
( Hash256
, Hash160
, FieldP
, FieldN
, Ring(..)
, RingMod(..)
, curveN
, curveP
, toFieldN
, toFieldP
, toMod256
, toMod160
, inverseP
, inverseN
, quadraticResidue
, isValidPrivkey
) where

import Data.Bits 
    ( Bits
    , (.&.), (.|.), xor
    , complement
    , shift, shiftL, shiftR
    , bit, testBit, bitSize
    , popCount, isSigned
    )
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( getWord64be
    , getWord32be
    , getWord8
    , getByteString
    , runGet
    , Get
    )
import Data.Binary.Put 
    ( putWord64be
    , putWord32be
    , putWord8
    , putByteString
    , runPut
    )
import Control.Monad (unless, guard)
import Control.Applicative ((<$>))
import Data.Ratio (numerator, denominator)
import Data.Word (Word8)
import qualified Data.ByteString as BS 
    ( ByteString
    , head , length
    , pack, unpack
    )

import Haskoin.Crypto.NumberTheory (mulInverse)
import Haskoin.Crypto.Util 
    ( toStrictBS
    , toLazyBS
    , bsToInteger
    , integerToBS
    )

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
    rFromInteger i = Ring $ i `mod` 2 ^ (256 :: Integer)
    rBitSize     _ = 256

instance RingMod Mod160 where
    rFromInteger i = Ring $ i `mod` 2 ^ (160 :: Integer)
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

instance RingMod n => Bounded (Ring n) where
    minBound = fromInteger 0
    maxBound = fromInteger (-1)
    
instance RingMod n => Real (Ring n) where
    toRational (Ring i) = toRational i

instance RingMod n => Enum (Ring n) where
    succ r@(Ring i)
        | r == maxBound = error "Ring: tried to take succ of maxBound"
        | otherwise = fromInteger $ succ i
    pred r@(Ring i) 
        | r == minBound = error "Ring: tried to take pred of minBound"
        | otherwise = fromInteger $ pred i
    toEnum i
        | toInteger i >= toInteger (minFrom r) && 
          toInteger i <= toInteger (maxFrom r) = r
        | otherwise = error "Ring: toEnum is outside of bounds"
        where r = fromInteger $ toEnum i
              minFrom :: RingMod a => Ring a -> Ring a
              minFrom _ = minBound
              maxFrom :: RingMod a => Ring a -> Ring a
              maxFrom _ = maxBound
    fromEnum (Ring i) = fromEnum i

instance RingMod n => Integral (Ring n) where
    (Ring i1) `quot` (Ring i2) = fromInteger $ i1 `quot` i2
    (Ring i1) `rem` (Ring i2) = fromInteger $ i1 `rem` i2
    (Ring i1) `div` (Ring i2) = fromInteger $ i1 `div` i2
    (Ring i1) `mod` (Ring i2) = fromInteger $ i1 `mod` i2
    (Ring i1) `quotRem` (Ring i2) = (fromInteger a, fromInteger b)
        where (a,b) = i1 `quotRem` i2
    (Ring i1) `divMod` (Ring i2) = (fromInteger a, fromInteger b)
        where (a,b) = i1 `divMod` i2
    toInteger (Ring i) = i

{- Fractional is only defined for prime orders -}

instance Fractional (Ring ModP) where
    recip = inverseP
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Fractional (Ring ModN) where
    recip = inverseN
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

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

-- DER encoding of a FieldN element as Integer
-- http://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf
instance Binary (Ring ModN) where
    get = do
        t <- getWord8
        unless (t == 0x02) (fail $
            "Bad DER identifier byte " ++ (show t) ++ ". Expecting 0x02" )
        l <- getWord8
        unless (l <= 33) (fail $
            "Bad DER length " ++ (show l) ++ ". Expecting length <= 33" )
        i <- bsToInteger <$> getByteString (fromIntegral l)
        unless (isValidPrivkey i) (fail $ "Invalid private key " ++ (show i))
        return $ fromInteger i

    put (Ring 0) = error "0 is an invalid FieldN element to serialize"
    put (Ring i) = do
        putWord8 0x02 -- Integer type
        let b = integerToBS i
            l = fromIntegral $ BS.length b
        if BS.head b >= 0x7f 
            then do
                putWord8 (l + 1)
                putWord8 0x00
            else do
                putWord8 l
        putByteString b

instance Binary (Ring ModP) where

    -- Section 2.3.6 http://www.secg.org/download/aid-780/sec1-v2.pdf
    get = do
        (Ring i) <- get :: Get Hash256
        unless (i < curveP) (fail $ "Get: Integer not in FieldP: " ++ (show i))
        return $ fromInteger i

    -- Section 2.3.7 http://www.secg.org/download/aid-780/sec1-v2.pdf
    put r = put $ toMod256 r
         

-- curveP = 3 (mod 4), thus Lagrange solutions apply
-- http://en.wikipedia.org/wiki/Quadratic_residue
quadraticResidue :: FieldP -> [FieldP]
quadraticResidue x = guard (y^2 == x) >> [y, (-y)]
    where q = (curveP + 1) `div` 4
          y = x^q

isValidPrivkey :: Integer -> Bool
isValidPrivkey i = i > 0 && i < curveN

