{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

main :: IO ()
main = defaultMain set6

set6 :: TestTree
set6 = testGroup "Cryptopals crypto challenges - Set 6"
    [ testCase "Set 6" $ assertFailure "No tests implemented"
    ]
