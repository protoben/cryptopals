module Main where

import Protolude

import Test.Tasty

import Challenges.Set1
import Challenges.Set2
import Challenges.Set3
import Challenges.Set4
import Challenges.Set5
import Challenges.Set6
import Challenges.Set7
import Challenges.Set8

main :: IO ()
main = defaultMain allSets

allSets :: TestTree
allSets = testGroup "Cryptopals crypto challenges"
    [set1, set2, set3, set4, set5, set6, set7, set8]
