module Haskoin.Crypto.ECDSA
( ECDSA
, Signature(..)
, signMessage
, verifyMessage
, withECDSA
) where

import Data.Maybe (fromJust)

import qualified Data.Binary as B (Binary, get, put)
import Data.Binary.Put 
    ( runPut
    , putWord8
    , putByteString
    )
import Data.Binary.Get (getWord8)

import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Monad (liftM, guard, unless)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.State 
    ( StateT
    , evalStateT
    , get, put
    )

import qualified Data.ByteString as BS (length)

import Haskoin.Crypto.Hash (doubleHash256)
import Haskoin.Crypto.Keys (PrivateKey, curveG)
import Haskoin.Crypto.Util 
    ( toStrictBS
    , isolate
    , integerToBS
    )
import Haskoin.Crypto.Point 
    ( Point
    , getAffine, makePoint
    , mulPoint, shamirsTrick
    )
import Haskoin.Crypto.Ring 
    ( Hash256
    , FieldN
    , toFieldN
    , toMod256
    )

type Nonce = FieldN

newtype ECDSA m a = ECDSA { runECDSA :: StateT Nonce m a }

instance Functor m => Functor (ECDSA m) where
    fmap f m = ECDSA $ fmap f (runECDSA m)

instance (Applicative m, Monad m) => Applicative (ECDSA m) where
    pure = return

    k <*> m = ECDSA $ do
        f <- runECDSA k
        x <- runECDSA m
        return $ f x


instance Monad m => Monad (ECDSA m) where
    m >>= f = ECDSA $ do
        x <- runECDSA m
        runECDSA $ f x

    return = ECDSA . return

instance MonadTrans ECDSA where
    lift = ECDSA . lift -- Lift over the StateT monad

data Signature = Signature { sigR :: FieldN, sigS :: FieldN }
    deriving (Show, Eq)

instance B.Binary Signature where
    get = do
        t <- getWord8
        -- 0x30 is DER sequence type
        unless (t == 0x30) (fail $ 
            "Bad DER identifier byte " ++ (show t) ++ ". Expecting 0x30")
        l <- getWord8
        -- Length = (33 + 1 identifier byte + 1 length byte) * 2
        unless (l <= 70) (fail $
            "Bad DER length " ++ (show t) ++ ". Expecting length <= 70")
        isolate (fromIntegral l) $ do
            Signature <$> B.get <*> B.get

    put (Signature 0 s) = error "0 is an invalid r value in a Signature"
    put (Signature r 0) = error "0 is an invalid s value in a Signature"
    put (Signature r s) = do
        putWord8 0x30
        let c = toStrictBS $ runPut $ B.put r >> B.put s
        putWord8 (fromIntegral $ BS.length c)
        putByteString c

-- The Integer here must be a strong random / pseudo-random number
withECDSA :: Monad m => Integer -> ECDSA m a -> m a
withECDSA i m = evalStateT (runECDSA m) (fromInteger i)
    
getNextNonce :: Monad m => ECDSA m Nonce
getNextNonce = ECDSA $ do
    -- Hash the nonce when we read it
    nonce <- hash `liftM` get
    -- Hash it again when we save it
    put $ hash nonce
    -- Make sure the nonce is not 0
    if nonce > 0 
        then return nonce 
        else runECDSA getNextNonce
    where 
        hash = toFieldN . doubleHash256 . toBS
        toBS = integerToBS . toInteger

-- Build a private/public key pair from the ECDSA monad random nonce
-- Section 3.2.1 http://www.secg.org/download/aid-780/sec1-v2.pdf
genKeyPair :: Monad m => ECDSA m (FieldN, Point)
genKeyPair = do
    -- 3.2.1.1 
    d <- getNextNonce
    -- 3.2.1.2
    let q = mulPoint d curveG
    -- 3.2.1.3
    return (d,q)

-- Safely sign a message inside the ECDSA monad.
-- ECDSA monad will generate a new nonce for each signature
-- Section 4.1.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
signMessage :: Monad m => Hash256 -> FieldN -> ECDSA m Signature
signMessage _ 0 = error "Integer 0 is an invalid private key"
signMessage h d = do
    -- 4.1.3.1
    (k,p) <- genKeyPair
    case unsafeSignMessage h d (k,p) of
        (Just sig) -> return sig
        -- If signing failed, retry with a new nonce
        Nothing    -> signMessage h d

-- Signs a message by providing the nonce
-- Re-using the same nonce twice will expose the private keys
-- Use signMessage within the ECDSA monad instead
-- Section 4.1.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
unsafeSignMessage :: Hash256 -> FieldN -> (FieldN, Point) -> Maybe Signature
unsafeSignMessage _ 0 _ = Nothing
unsafeSignMessage h d (k,p) = do
    -- 4.1.3.1 (4.1.3.2 not required)
    (x,_) <- getAffine p
    -- 4.1.3.3
    let r = toFieldN x
    guard (r /= 0)
    -- 4.1.3.4 / 4.1.3.5
    let e = toFieldN h
    -- 4.1.3.6
    let s' = (e + r*d)/k
        -- Only create signatures with even s
        s  = if even s' then s' else (-s')
    guard (s /= 0)
    -- 4.1.3.7
    return $ Signature r s

-- Section 4.1.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
verifyMessage :: Hash256 -> Signature -> Point -> Bool
-- 4.1.4.1 (r and s can not be zero)
verifyMessage _ (Signature 0 _) _ = False
verifyMessage _ (Signature _ 0) _ = False
verifyMessage h (Signature r s) q = 
    case getAffine p of
        Nothing      -> False
        -- 4.1.4.7 / 4.1.4.8
        (Just (x,_)) -> (toFieldN x) == r
    where 
        -- 4.1.4.2 / 4.1.4.3
        e  = toFieldN h
        -- 4.1.4.4
        u1 = e/s
        u2 = r/s
        -- 4.1.4.5 (u1*G + u2*q)
        p  = shamirsTrick u1 curveG u2 q

