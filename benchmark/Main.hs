{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock
import System.Random
import Control.Monad
import Control.Applicative

import Haskoin.Crypto.ECDSA
import Haskoin.Crypto.Keys
import Haskoin.Crypto.Point
import Haskoin.Crypto.Ring

bench :: Int -> String -> IO a -> IO a
bench n s f = do
    start <- getCurrentTime
    !r <- f
    end <- getCurrentTime
    let t = (diffUTCTime end start)
    putStrLn $ "----------------------------"
    putStrLn $ s ++ " (" ++ (show n) ++ " samples)"
    putStrLn $ "Total time: " ++ (show t)
    putStrLn $ "Op/sec    : " ++ (show $ (fromIntegral n)/t)
    return r

main = do

    bench (10^7) "Ring multiplication (mod n)" (return $ testRing (10^7))
    bench (10^5) "Ring inversion (mod n)" (return $ invRing (10^5))

    let elems = 2000
        msg   = fromInteger $ curveN - 10

    !priv <- replicateM elems $
                makePrivateKey <$> getStdRandom (randomR (1, curveN))

    !pub <- bench elems "Point multiplications" $ forM priv $ \x -> 
        return $! derivePublicKey x

    bench 100000 "Point additions" $ 
        forM (take 100000 $ cycle pub) $ \x -> do
            let !a = runPublicKey x
            return $! addPoint a a

    bench 100000 "Point doubling" $ 
        forM (take 100000 $ cycle pub) $ \x -> do
            let !a = runPublicKey x
            return $! doublePoint a

    bench elems "Shamirs trick" $ 
        forM (priv `zip` pub) $ \(d,q) -> do
            let !a = runPrivateKey d
                !b = runPublicKey q
            return $! shamirsTrick a b a b

    !sigs <- bench elems "Signature creations" $ 
        withECDSA 1 $! forM priv (signMessage msg) 
        
    bench elems "Signature verifications" $ 
        forM (sigs `zip` pub) $ \(s,q) -> 
            return $! verifyMessage msg s q

testRing :: Int -> FieldN
testRing max = go 2 0
    where go i n
            | n < max = go (i*i) (n + 1)
            | otherwise = i

invRing :: Int -> FieldN
invRing max = go 1 0
    where go i n
            | n < max = go ((inverseN i) + 1) (n + 1)
            | otherwise = i

