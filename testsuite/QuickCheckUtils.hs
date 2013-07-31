module QuickCheckUtils where

import Control.Applicative ((<$>), (<*>))

import Test.QuickCheck

import Point
import Ring
import ECDSA
import NumberTheory

instance RingMod n => Arbitrary (Ring n) where
    arbitrary = fromInteger <$> (arbitrary :: Gen Integer)

instance Arbitrary Point where
    arbitrary = frequency
        [ (1, return makeInfPoint)
        , (9, (mulPoint curveG) <$> (arbitrary :: Gen FieldN))
        ]

