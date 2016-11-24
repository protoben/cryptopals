{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

main :: IO ()
main = defaultMain set8

set8 :: TestTree
set8 = testGroup "Cryptopals crypto challenges - Set 8"
    [ testCase "Set 8" $ assertFailure "No tests implemented"
    ]
