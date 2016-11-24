{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

main :: IO ()
main = defaultMain set7

set7 :: TestTree
set7 = testGroup "Cryptopals crypto challenges - Set 7"
    [ testCase "Set 7" $ assertFailure "No tests implemented"
    ]
