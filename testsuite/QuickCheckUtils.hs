module QuickCheckUtils where

import Control.Applicative ((<$>), (<*>))

import Test.QuickCheck

import Point
import Ring
import ECDSA
import NumberTheory

data Mod32
type Test32  = Ring Mod32

instance RingMod Mod32 where
    rFromInteger i = Ring $ i `mod` 2^32
    rMod         _ = 2^32
    rBitSize     _ = 32

instance RingMod n => Arbitrary (Ring n) where
    arbitrary = fromInteger <$> (arbitrary :: Gen Integer)

instance Arbitrary Point where
    arbitrary = frequency
        [ (1, return makeInfPoint)
        , (9, (mulPoint curveG) <$> (arbitrary :: Gen FieldN))
        ]

