{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Cryptopals

main :: IO ()
main = defaultMain set4

set4 :: TestTree
set4 = testGroup "Cryptopals crypto challenges - Set 4"
    [ testCase "Set 4" $ assertFailure "No tests implemented"
    ]
