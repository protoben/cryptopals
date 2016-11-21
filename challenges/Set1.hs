{-# LANGUAGE OverloadedStrings #-}
module Set1 where

import Protolude

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as S

import Cryptopals

set1 :: TestTree
set1 = testGroup "Set 1"
    [ testCase  "Challenge 1: convert hex to base64" $
        base64 (Hex $ S.concat
            [ "49276d206b696c6c696e6720796f7572"
            , "20627261696e206c696b65206120706f"
            , "69736f6e6f7573206d757368726f6f6d"
            ])
        @=? (Base64 $ S.concat
            [ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBs"
            , "aWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
            ])
    ]
