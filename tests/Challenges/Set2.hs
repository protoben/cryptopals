{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

main :: IO ()
main = defaultMain set2

set2 :: TestTree
set2 = testGroup "Cryptopals crypto challenges - Set 2"
    [ testCase "Set 2" $ assertFailure "No tests implemented"
    ]
