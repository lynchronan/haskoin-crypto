module ECDSA
( ECDSA
, PublicKey
, PrivateKey
, curveG
, curveH
, signMessage
, withNonceDo
) where

import Data.Maybe
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Binary as B

import Control.Monad.State
import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Hash
import Point 
import Ring 

type PublicKey = Point
type PrivateKey = FieldN
type Signature = (FieldN,FieldN)
type Nonce = FieldN
type KeyPair = (PrivateKey,PublicKey)

curveG :: Point
curveG = fromJust $ makePoint
        0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798       
        0X483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8 

curveH :: Integer
curveH = 0x01

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
signMessage :: Monad m => BS.ByteString -> PrivateKey -> ECDSA m Signature
signMessage _ 0 = error "Integer 0 is an invalid private key"
signMessage m d = do
    -- 4.1.3.1
    (k,p) <- genKeyPair
    case unsafeSignMessage m d (k,p) of
        (Just sig) -> return sig
        -- If signing failed, retry with a new nonce
        Nothing    -> signMessage m d

-- Signs a message by providing the nonce
-- Re-using the same nonce twice will expose the private keys
-- Use signMessage within the ECDSA monad instead
-- Section 4.1.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
unsafeSignMessage :: BS.ByteString -> PrivateKey -> KeyPair -> Maybe Signature
unsafeSignMessage _ 0 _ = Nothing
unsafeSignMessage m d (k,p) = do
    -- 4.1.3.1 (4.1.3.2 not required)
    (x,y) <- getAffine p
    -- 4.1.3.3
    let r = toFieldN x
    guard (r /= 0)
    -- 4.1.3.4 / 4.1.3.5
    let e = toFieldN $ doubleSHA256 m
    -- 4.1.3.6
    let s = (e + r*d)/k
    guard (s /= 0)
    -- 4.1.3.7
    return (r,s)

-- Section 4.1.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
verifyMessage :: BS.ByteString -> Signature -> PublicKey -> Bool
-- 4.1.4.1
verifyMessage _ (0,_) _ = False
verifyMessage _ (_,0) _ = False
verifyMessage m (r,s) p = 
        -- 4.1.4.2 / 4.1.4.3
    let e  = toFieldN $ doubleSHA256 m
        -- 4.1.4.4
        u1 = e/s
        u2 = r/s
        -- 4.1.4.5
        pR = addPoint (mulPoint u1 curveG) (mulPoint u2 p)
        in case getAffine pR of
            Nothing      -> False
                            -- 4.1.4.7
            (Just (x,y)) -> let v = toFieldN x
                            -- 4.1.4.8
                            in v == r


