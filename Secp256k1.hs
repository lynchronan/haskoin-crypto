module Secp256k1 where

import Control.Monad

-- Elliptic curves of the form y^2 = x^3 + A*x + B (mod P)
-- secp256k1 curve parameters (p,a,b,g,n,h) used in Bitcoin

p :: Integer
p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f 

a :: Integer
a = 0x00

b :: Integer
b = 0x07

g :: Integer
g = 0x0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798 

n :: Integer
n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 

h :: Integer
h = 0x01

-- Defines a point in Jacobian coordinates (x=X/Z^2, y=Y/Z^3)
-- Z = 0 is a point at infinity but we use InfPoint for more clarity
data Point = Point Integer Integer Integer | InfPoint
    deriving Show

instance Eq Point where
    (Point x1 y1 z1) == (Point x2 y2 z2) = 
        ((x1*z2^2) `mod` p == (x2*z1^2) `mod` p) && 
        ((y1*z2^3) `mod` p == (y2*z1^3) `mod` p)

-- check if the point lies on the secp256k1 curve
checkPoint :: Point -> Bool
checkPoint point = case point of
    (Point 0 0 0) -> False
    (Point x y z) -> (y^2) `mod` p  == (x^3 + b*z^6) `mod` p -- a = 0

mkPoint :: Integer -> Integer -> Maybe Point
mkPoint x y
    | checkPoint point = Just point
    | otherwise = Nothing
    where point = Point x y 1

addPoint :: Point -> Point -> Point
addPoint InfPoint p = p
addPoint p InfPoint = p
addPoint p1@(Point x1 y1 z1) p2@(Point x2 y2 z2)
    | u1 == u2 = if s1 == s2 then doublePoint p1 else InfPoint
    | otherwise = Point x3 y3 z3
    where u1 = (x1*z2^2) `mod` p
          u2 = (x2*z1^2) `mod` p
          s1 = (y1*z2^3) `mod` p
          s2 = (y2*z1^3) `mod` p
          h  = u2 - u1 `mod` p
          r  = s2 - s1 `mod` p
          x3 = (r^2 - h^3 - 2*u1*h^2) `mod` p
          y3 = (r*(u1 * h^2 - x3) - s1 * h^3) `mod` p
          z3 = (h * z1 * z2) `mod` p

doublePoint :: Point -> Point
doublePoint (Point x y z)
    | y == 0 = InfPoint
    | otherwise = Point x' y' z'
    where s = (4*x*y^2) `mod` p
          m = (3*x^2) `mod` p -- a = 0
          x' = (m^2 - 2*s) `mod` p
          y' = (m*(s - x') - 8*y^4) `mod` p
          z' = (2*y*z) `mod` p
    
