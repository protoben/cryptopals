{-# LANGUAGE OverloadedStrings #-}
module Challenges.Set2 where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

set2 :: TestTree
set2 = testGroup "Crypto challenge set 2"
    [ testCase "Set 2" $ assertFailure "No tests implemented"
    ]
