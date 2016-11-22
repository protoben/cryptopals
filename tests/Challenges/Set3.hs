{-# LANGUAGE OverloadedStrings #-}
module Challenges.Set3 where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

set3 :: TestTree
set3 = testGroup "Crypto challenge set 3"
    [ testCase "Set 3" $ assertFailure "No tests implemented"
    ]
