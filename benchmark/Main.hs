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

msg :: Hash256
msg = fromInteger $ curveN - 10

main = do

    !priv <- replicateM 5000 $ 
                makePrivateKey <$> getStdRandom (randomR (1, curveN))

    startA <- getCurrentTime
    !pub <- mapM (\x -> return $! derivePublicKey x) priv
    endA <- getCurrentTime

    let tA = (diffUTCTime endA startA)
    putStrLn $ "5000 point multiplications took: " ++ show tA
    putStrLn $ "Point multiplications per second: " ++ (show $ 5000/tA)

    startB <- getCurrentTime
    !sigs <- withECDSA 1 $ mapM (signMessage msg) priv
    endB <- getCurrentTime

    let tB = (diffUTCTime endB startB)
    putStrLn $ "5000 signatures took: " ++ show tB
    putStrLn $ "Signatures per second: " ++ (show $ 5000/tB)

    let !zipped = sigs `zip` pub

    startC <- getCurrentTime
    !val <- mapM (\(s,q) -> return $! verifyMessage msg s q) zipped
    endC <- getCurrentTime

    let tC = (diffUTCTime endC startC)
    putStrLn $ "5000 verifications took: " ++ show tC
    putStrLn $ "Verification per second: " ++ (show $ 5000/tC)
    putStrLn $ "Verification status: " ++ (show $ and val)

