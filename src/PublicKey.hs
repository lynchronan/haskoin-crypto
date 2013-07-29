module PublicKey
( PublicKey
, Point(..)
, Fp(..)
, makePoint
, addPoint
, doublePoint
, checkPoint
, getAffine
, getX
, getY
) where

import Control.Applicative ((<$>))
import Data.Ratio (numerator, denominator)
import NumberTheory (extendedModGCD, mulInverse)

type PublicKey = Point

-- Elliptic curves of the form y^2 = x^3 + A*x + B (mod P)

p :: Integer
p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f 

{-
a :: Integer
a = 0x00
-}

b :: Fp
b = 0x07

newtype Fp = Fp { runFp :: Integer } 
    deriving (Eq, Show)

--Point coordinates are stored modulo p
instance Num Fp where
    fromInteger a = Fp $ a `mod` p
    (Fp a) + (Fp b) = fromInteger $ a + b
    negate (Fp a) = fromInteger $ negate a
    (Fp a) * (Fp b) = fromInteger $ a * b
    abs _ = error "abs is not defined for EC point coordinates"
    signum _ = error "signum is not defined for EC point coordinates"

-- a divides b if there exists c such that b = ac (mod p)
instance Fractional Fp where
    recip (Fp a) = fromInteger $ mulInverse a p
    fromRational r = (fromInteger (numerator r)) / (fromInteger (denominator r))

-- Point in Jacobian coordinates (x=X/Z^2, y=Y/Z^3)
-- Z = 0 is a point at infinity but we use InfPoint for more clarity
data Point = Point Fp Fp Fp | InfPoint
    deriving Show
    
instance Eq Point where
    (Point x1 y1 z1) == (Point x2 y2 z2) = 
        (x1*z2^2 == x2*z1^2) && (y1*z2^3 == y2*z1^3)

makePoint :: Fp -> Fp -> Maybe Point
makePoint x y
    | checkPoint point = Just point
    | otherwise = Nothing
    where point = Point x y 1

-- check if the point lies on the secp256k1 curve
checkPoint :: Point -> Bool
checkPoint point = case point of
    (Point 0 0 0) -> False
    (Point x y z) -> (y^2) == x^3 + b*z^6 -- a = 0

getAffine :: Point -> Maybe (Fp, Fp)
getAffine point = case point of
    InfPoint      -> Nothing
    (Point _ _ 0) -> Nothing
    (Point x y z) -> Just (x/z^2, y/z^3)

getX :: Point -> Maybe Fp
getX point = fst <$> (getAffine point)

getY :: Point -> Maybe Fp
getY point = snd <$> (getAffine point)

addPoint :: Point -> Point -> Point
addPoint InfPoint point = point
addPoint point InfPoint = point
addPoint p1@(Point x1 y1 z1) p2@(Point x2 y2 z2)
    | u1 == u2 = if s1 == s2 then doublePoint p1 else InfPoint
    | otherwise = Point x3 y3 z3
    where u1 = x1*z2^2
          u2 = x2*z1^2
          s1 = y1*z2^3
          s2 = y2*z1^3
          h  = u2 - u1
          r  = s2 - s1
          x3 = r^2 - h^3 - 2*u1*h^2 
          y3 = r*(u1 * h^2 - x3) - s1 * h^3
          z3 = h * z1 * z2

doublePoint :: Point -> Point
doublePoint InfPoint = InfPoint
doublePoint (Point x y z)
    | y == 0 = InfPoint
    | otherwise = Point x' y' z'
    where s = 4*x*y^2
          m = 3*x^2 -- a = 0
          x' = m^2 - 2*s
          y' = m*(s - x') - 8*y^4
          z' = 2*y*z

