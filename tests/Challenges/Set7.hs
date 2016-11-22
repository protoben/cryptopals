{-# LANGUAGE OverloadedStrings #-}
module Challenges.Set7 where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

set7 :: TestTree
set7 = testGroup "Crypto challenge set 7"
    [ testCase "Set 7" $ assertFailure "No tests implemented"
    ]
