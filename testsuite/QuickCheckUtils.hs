{-# LANGUAGE EmptyDataDecls #-}
module QuickCheckUtils where

import Test.QuickCheck

import Control.Monad.Identity
import Control.Applicative ((<$>),(<*>))

import qualified Data.ByteString as BS

import Haskoin.Crypto.Point
import Haskoin.Crypto.Ring
import Haskoin.Crypto.ECDSA
import Haskoin.Crypto.Keys

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

instance Arbitrary PublicKey where
    arbitrary = oneof
        [ PublicKey  <$> (arbitrary :: Gen Point)
        , PublicKeyU <$> (arbitrary :: Gen Point)
        ]

instance Arbitrary PrivateKey where
    arbitrary = oneof
        [ PrivateKey  <$> (fromInteger <$> choose (1, curveN-1))
        , PrivateKeyU <$> (fromInteger <$> choose (1, curveN-1))
        ]

instance Arbitrary Signature where
    arbitrary = do
        i <- arbitrary :: Gen Integer
        d <- arbitrary :: Gen FieldN
        h <- arbitrary :: Gen Hash256
        let d' = if d == 0 then 1 else d
        return $ runIdentity $ withECDSA i (signMessage h d')

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return (BS.drop n bs) -- to give us some with non-0 offset

