module Main where

import Protolude

import Test.Tasty

import Set1

main :: IO ()
main = defaultMain allSets

allSets :: TestTree
allSets = testGroup "Cryptopals exercise sets"
    [set1]
