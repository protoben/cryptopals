{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

main :: IO ()
main = defaultMain set3

set3 :: TestTree
set3 = testGroup "Cryptopals crypto challenges - Set 3"
    [ testCase "Set 3" $ assertFailure "No tests implemented"
    ]
