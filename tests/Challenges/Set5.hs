{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

main :: IO ()
main = defaultMain set5

set5 :: TestTree
set5 = testGroup "Cryptopals crypto challenges - Set 5"
    [ testCase "Set 5" $ assertFailure "No tests implemented"
    ]
