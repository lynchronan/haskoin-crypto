module ECDSA
( ECDSA
, PublicKey
, PrivateKey
, curveG
, signMessage
, verifyMessage
, withNonceDo
) where

import Data.Maybe (fromJust)
import Data.Binary.Put (runPut)
import qualified Data.Binary as B (put)
import Control.Monad (liftM, guard)
import Control.Monad.State 
    ( StateT
    , evalStateT
    , get, put
    )

import Hash (doubleSHA256, toStrictBS)
import Point 
    ( Point
    , getAffine, makePoint
    , mulPoint, shamirsTrick
    )
import Ring 
    ( Hash256
    , FieldN
    , toFieldN
    , toMod256
    )

type PublicKey = Point
type PrivateKey = FieldN
type Signature = (FieldN,FieldN)
type Nonce = FieldN
type KeyPair = (PrivateKey,PublicKey)

curveG :: Point
curveG = fromJust $ makePoint
        0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798       
        0X483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8 

newtype ECDSA m a = ECDSA { runECDSA :: StateT Nonce m a }

instance Monad m => Monad (ECDSA m) where
    m >>= f = ECDSA $ do
        x <- runECDSA m
        runECDSA $ f x

    return = ECDSA . return

instance (Functor m, Monad m) => Functor (ECDSA m) where
    f `fmap` m = ECDSA $ f `fmap` (runECDSA m)

withNonceDo :: Monad m => Integer -> ECDSA m a -> m a
withNonceDo i m = evalStateT (runECDSA m) (fromInteger i)
    
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
        hash = toFieldN . doubleSHA256 . toStrictBS . runPut . B.put . toMod256

-- Build a private/public key pair from the ECDSA monad random nonce
-- Section 3.2.1 http://www.secg.org/download/aid-780/sec1-v2.pdf
genKeyPair :: Monad m => ECDSA m KeyPair
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
signMessage :: Monad m => Hash256 -> PrivateKey -> ECDSA m Signature
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
unsafeSignMessage :: Hash256 -> PrivateKey -> KeyPair -> Maybe Signature
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
    let s = (e + r*d)/k
    guard (s /= 0)
    -- 4.1.3.7
    return (r,s)

-- Section 4.1.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
verifyMessage :: Hash256 -> Signature -> PublicKey -> Bool
-- 4.1.4.1 (r and s can not be zero)
verifyMessage _ (0,_) _ = False
verifyMessage _ (_,0) _ = False
verifyMessage h (r,s) q = 
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

