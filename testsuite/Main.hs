module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Haskoin.Crypto.Ring.Tests (tests)
import qualified Haskoin.Crypto.Point.Tests (tests)
import qualified Haskoin.Crypto.ECDSA.Tests (tests)
import qualified Haskoin.Crypto.Base58.Tests (tests)
import qualified Haskoin.Crypto.Keys.Tests (tests)
import qualified Units (tests)

main = defaultMain
    (  Haskoin.Crypto.Ring.Tests.tests 
    ++ Haskoin.Crypto.Point.Tests.tests 
    ++ Haskoin.Crypto.ECDSA.Tests.tests 
    ++ Haskoin.Crypto.Base58.Tests.tests 
    ++ Haskoin.Crypto.Keys.Tests.tests 
    ++ Units.tests
    )

