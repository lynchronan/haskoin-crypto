module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import PublicKey.Tests

main = defaultMain PublicKey.Tests.tests
