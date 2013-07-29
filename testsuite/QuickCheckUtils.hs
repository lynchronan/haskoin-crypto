module QuickCheckUtils where

import Control.Applicative ((<$>), (<*>))

import Test.QuickCheck

import Secp256k1
import PublicKey
import PrivateKey
import NumberTheory

instance Arbitrary Fn where
    arbitrary = fromInteger <$> (arbitrary :: Gen Integer)

instance Arbitrary Fp where
    arbitrary = fromInteger <$> (arbitrary :: Gen Integer)

instance Arbitrary Point where
    arbitrary = frequency
        [ (1, return InfPoint)
        , (9, mulWithGen <$> (arbitrary :: Gen Fn))
        ]


