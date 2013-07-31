module ECDSA
( PublicKey
, PrivateKey
, curveG
, curveH
) where

import qualified Data.ByteString as BS

import Data.Maybe (fromJust)
import Point (Point, makePoint)
import Ring (FieldN)

type PublicKey = Point
type PrivateKey = FieldN
type Signature = (FieldN,FieldN)

curveG :: Point
curveG = fromJust $ makePoint
        0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798       
        0X483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8 

curveH :: Integer
curveH = 0x01

--signMessage :: Hash -> PrivateKey -> Signature
--signMessage m d = 

