module Haskoin.Crypto.Keys
( PublicKey(..)
, PrivateKey(..)
, derivePublicKey
, publicKeyAddress
, makePrivateKey
, makePrivateKeyU
, isCompressed
, isPrivateKeyCompressed
, fromWIF
, toWIF
, curveG
) where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8, runPut)

import Control.Monad (when, unless, guard)
import Control.Applicative ((<$>),(<*>))
import Data.Maybe (isJust, fromJust)

import qualified Data.ByteString as BS 
    ( ByteString
    , head, tail
    , last, init
    , cons, snoc
    , length
    )
import Haskoin.Crypto.Ring 
    ( FieldN, FieldP
    , isValidPrivkey
    , quadraticResidue
    , toMod256
    )
import Haskoin.Crypto.Point 
    ( Point( InfPoint )
    , makePoint
    , mulPoint 
    , getAffine
    , curveB
    )
import Haskoin.Crypto.Base58 (encodeBase58Check, decodeBase58Check)
import Haskoin.Crypto.Hash (hash160BS, hash256BS)
import Haskoin.Crypto.Util (toStrictBS, bsToInteger)

curveG :: Point
curveG = fromJust $ makePoint
        0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798       
        0X483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8 

data PublicKey =
        PublicKey { runPublicKey :: Point } | -- default is Compressed
        PublicKeyU { runPublicKey :: Point }  -- Uncompressed is explicit
        deriving Show

instance Eq PublicKey where
    -- Compression does not matter for InfPoint
    (PublicKey  InfPoint) == (PublicKeyU InfPoint) = True
    (PublicKeyU InfPoint) == (PublicKey InfPoint) = True
    a == b = (runPublicKey a) == (runPublicKey b)

data PrivateKey =
        PrivateKey { runPrivateKey :: FieldN } | -- default is Compressed
        PrivateKeyU { runPrivateKey :: FieldN }  -- Uncompressed is explicit
        deriving (Show, Eq)

derivePublicKey :: PrivateKey -> PublicKey
derivePublicKey k = case k of
    (PrivateKey  d) -> PublicKey  $ mulPoint d curveG
    (PrivateKeyU d) -> PublicKeyU $ mulPoint d curveG

-- Integer needs to be a random number with at least 256 bits of entropy
makePrivateKey :: Integer -> PrivateKey
makePrivateKey i
    | isValidPrivkey i = PrivateKey $ fromInteger i
    | otherwise         = error $ "Invalid private key: " ++ (show i)

-- Integer needs to be a random number with at least 256 bits of entropy
makePrivateKeyU :: Integer -> PrivateKey
makePrivateKeyU i
    | isValidPrivkey i = PrivateKeyU $ fromInteger i
    | otherwise         = error $ "Invalid private key: " ++ (show i)

isCompressed :: PublicKey -> Bool
isCompressed (PublicKey  _) = True
isCompressed (PublicKeyU _) = False

isPrivateKeyCompressed :: PrivateKey -> Bool
isPrivateKeyCompressed (PrivateKey  _) = True
isPrivateKeyCompressed (PrivateKeyU _) = False
    
instance Binary PublicKey where

    -- Section 2.3.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
    get = go =<< getWord8
              -- 2.3.4.1 InfPoint if input is 0x00
        where go 0 = return $ PublicKey InfPoint
              -- 2.3.4.3 Uncompressed format
              go 4 = getUncompressed
              -- 2.3.4.2 Compressed format
              -- 2 means pY is even, 3 means pY is odd
              go y | y == 2 || y == 3 = getCompressed (even y)
                   | otherwise = fail "Get: Invalid public key encoding"

    -- Section 2.3.3 http://www.secg.org/download/aid-780/sec1-v2.pdf
    put pk = case getAffine (runPublicKey pk) of
        -- 2.3.3.1
        Nothing -> putWord8 0x00
        (Just (x,y)) -> case pk of
            -- Compressed
            (PublicKey  p) -> putWord8 (if even y then 2 else 3) >> put x
            -- Uncompressed
            (PublicKeyU p) -> putWord8 4 >> put x >> put y

getUncompressed :: Get PublicKey
getUncompressed = do
    p <- makePoint <$> get <*> get
    unless (isJust p) (fail "Get: Point not on the curve")
    return $ PublicKeyU $ fromJust $ p

getCompressed :: Bool -> Get PublicKey
getCompressed e = do
    -- 2.1 
    x <- get :: Get FieldP
    -- 2.4.1 (deriving yP)
    let a  = x ^ (3 :: Integer) + curveB
        ys = filter matchSign (quadraticResidue a)
    -- We found no square root (mod p)
    when (null ys) (fail $ "No ECC point for x = " ++ (show x))
    let p = makePoint x (head ys)
    -- Additionally, check that the point is on the curve
    unless (isJust p) (fail "Get: Point not on the curve")
    return $ PublicKey $ fromJust $ p
    where matchSign a = (even a) == e

publicKeyAddress :: PublicKey -> BS.ByteString
publicKeyAddress p = 
    encodeBase58Check . (BS.cons 0x00) . hash160BS . hash256BS $ bs
    where bs = toStrictBS $ runPut $ put p 

fromWIF :: BS.ByteString -> Maybe PrivateKey
fromWIF bs = do
    b <- decodeBase58Check bs
    guard (BS.head b == 0x80)  -- Check that this is a private key
    case BS.length b of
        33 -> do               -- Uncompressed format
            let i = bsToInteger (BS.tail b)
            guard (isValidPrivkey i)   
            return $ PrivateKeyU $ fromInteger i
        34 -> do               -- Compressed format
            guard (BS.last b == 0x01) 
            let i = bsToInteger $ BS.tail $ BS.init b
            guard (isValidPrivkey i)
            return $ PrivateKey $ fromInteger i
        _  -> Nothing          -- Bad length

toWIF :: PrivateKey -> BS.ByteString
toWIF k = case k of
    (PrivateKey  0) -> error "0 is an invalid private key to export"
    (PrivateKeyU 0) -> error "0 is an invalid private key to export"
    (PrivateKey  d) -> encodeBase58Check $ BS.cons 0x80 (BS.snoc (bs d) 0x01)
    (PrivateKeyU d) -> encodeBase58Check $ BS.cons 0x80 (bs d)
    where bs x = toStrictBS $ runPut $ put $ toMod256 x

