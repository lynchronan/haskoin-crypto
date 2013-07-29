module QuickCheckUtils where

import Control.Applicative ((<$>), (<*>))

import Test.QuickCheck

import Secp256k1
import PublicKey
import PrivateKey
import NumberTheory

instance Arbitrary Fp where
    arbitrary = Fp <$> arbitrary

instance Arbitrary Point where
    arbitrary = Point <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary

