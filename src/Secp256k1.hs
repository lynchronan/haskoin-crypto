module Secp256k1
( addPoint
, mulPoint
, mulWithGen
, doublePoint
) where

import Data.Maybe (fromJust)

import PublicKey 
    ( Point(Point,InfPoint)
    , PublicKey
    , doublePoint
    , addPoint
    , makePoint
    , getAffine , getX, getY
    )
import PrivateKey (PrivateKey ,runFn)
import Data.Bits (shiftR, testBit)

-- Elliptic curves of the form y^2 = x^3 + A*x + B (mod P)
-- secp256k1 curve parameters (p,a,b,g,n,h) used in Bitcoin

g :: PublicKey
g = fromJust $ makePoint
        0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798       
        0X483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8 

--g = 0x0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798 

h :: Integer
h = 0x01

mulWithGen :: PrivateKey -> Point
mulWithGen = mulPoint g

-- Point multiplication using Montgomery ladder
mulPoint :: Point -> PrivateKey -> Point
mulPoint point 0 = InfPoint
mulPoint point 1 = point
mulPoint InfPoint _ = InfPoint
mulPoint point n = go InfPoint point ((msbPos n) - 1)
    where go r0 r1 i
            | i < 0       = r0
            | testBit (runFn n) i = go (addPoint r0 r1) (doublePoint r1) (i - 1)
            | otherwise   = go (doublePoint r0) (addPoint r0 r1) (i - 1)

-- Calculates the position of the most significant bit in this Integer
msbPos :: PrivateKey -> Int
msbPos n = go (runFn n) 0
    where go i pos
            | i == 0    = pos
            | otherwise = go (i `shiftR` 1) (pos + 1)
    
