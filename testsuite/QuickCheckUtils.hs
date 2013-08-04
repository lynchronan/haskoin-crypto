{-# LANGUAGE EmptyDataDecls #-}
module QuickCheckUtils where

import Test.QuickCheck

import Control.Monad.Identity
import Control.Applicative ((<$>),(<*>))

import Point
import Ring
import ECDSA

data Mod32
type Test32  = Ring Mod32

instance RingMod Mod32 where
    rFromInteger i = Ring $ i `mod` 2 ^ (32 :: Integer)
    rBitSize     _ = 32

instance RingMod n => Arbitrary (Ring n) where
    arbitrary = fromInteger <$> (arbitrary :: Gen Integer)

instance Arbitrary Point where
    arbitrary = frequency
        [ (1, return makeInfPoint)
        , (9, (flip mulPoint $ curveG) <$> (arbitrary :: Gen FieldN))
        ]

instance Arbitrary Signature where
    arbitrary = do
        i <- arbitrary :: Gen Integer
        d <- arbitrary :: Gen PrivateKey
        h <- arbitrary :: Gen Hash256
        let d' = if d == 0 then 1 else d
        return $ runIdentity $ withECDSA i (signMessage h d')

