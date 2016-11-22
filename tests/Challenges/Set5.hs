{-# LANGUAGE OverloadedStrings #-}
module Challenges.Set5 where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

set5 :: TestTree
set5 = testGroup "Crypto challenge set 5"
    [ testCase "Set 5" $ assertFailure "No tests implemented"
    ]
