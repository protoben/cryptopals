{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cryptopals.Statistics where

import Protolude

import qualified Data.ByteString.Char8 as S
import Language.Words (allWords)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((&&&))

import Cryptopals.Encoding

type CharRanks = Map Char Double

rankCharsByFreq :: Text -> CharRanks
rankCharsByFreq = Text.foldr (Map.update $ Just . (+1)) emptyRanks

emptyRanks, asciiRanks, dictRanks, corpusRanks, ranks :: CharRanks
emptyRanks  = Map.fromList $ zip ['\x00'..'\xff'] (repeat 0)
asciiRanks  = foldr (uncurry Map.insert) emptyRanks $ zip ['\x7f'..'\xff'] (repeat (-9999))
dictRanks   = rankCharsByFreq . Text.concat $ allWords
ranks       = corpusRanks
corpusRanks = foldr (uncurry Map.insert) emptyRanks
    [ ('\n',1702.0)
    , ('\r',1702.0)
    , (' ',12387.0)
    , ('!',176.0)
    , ('"',710.0)
    , ('#',1.0)
    , ('$',2.0)
    , ('%',1.0)
    , ('&',2.0)
    , ('\'',260.0)
    , ('(',33.0)
    , (')',33.0)
    , ('*',33.0)
    , (',',927.0)
    , ('-',287.0)
    , ('.',621.0)
    , ('/',31.0)
    , ('0',24.0)
    , ('1',64.0)
    , ('2',10.0)
    , ('3',21.0)
    , ('4',9.0)
    , ('5',12.0)
    , ('6',9.0)
    , ('7',6.0)
    , ('8',11.0)
    , ('9',15.0)
    , (':',40.0)
    , (';',78.0)
    , ('?',70.0)
    , ('@',2.0)
    , ('A',352.0)
    , ('B',62.0)
    , ('C',100.0)
    , ('D',120.0)
    , ('E',187.0)
    , ('F',92.0)
    , ('G',122.0)
    , ('H',92.0)
    , ('I',375.0)
    , ('J',14.0)
    , ('K',42.0)
    , ('L',90.0)
    , ('M',79.0)
    , ('N',113.0)
    , ('O',116.0)
    , ('P',140.0)
    , ('Q',34.0)
    , ('R',133.0)
    , ('S',159.0)
    , ('T',241.0)
    , ('U',53.0)
    , ('V',20.0)
    , ('W',103.0)
    , ('X',4.0)
    , ('Y',58.0)
    , ('Z',0.0)
    , ('[',27.0)
    , (']',27.0)
    , ('_',150.0)
    , ('a',4058.0)
    , ('b',742.0)
    , ('c',1435.0)
    , ('d',2245.0)
    , ('e',6671.0)
    , ('f',1061.0)
    , ('g',1230.0)
    , ('h',3122.0)
    , ('i',3547.0)
    , ('j',129.0)
    , ('k',509.0)
    , ('l',2221.0)
    , ('m',1064.0)
    , ('n',3530.0)
    , ('o',4242.0)
    , ('p',849.0)
    , ('q',62.0)
    , ('r',3111.0)
    , ('s',3057.0)
    , ('t',5152.0)
    , ('u',1634.0)
    , ('v',389.0)
    , ('w',1165.0)
    , ('x',70.0)
    , ('y',985.0)
    , ('z',34.0)
    ]

rankChar :: Char -> Double
rankChar = maybe 0 identity . (`Map.lookup` ranks)

rankString :: (Encoding e ByteString) => e ByteString -> Double
rankString s = (/l) . sum . fmap rankChar . S.unpack $ bs
    where l  = fromIntegral . S.length $ bs
          bs = toRaw s

bestString :: (Encoding e ByteString) => [e ByteString] -> e ByteString
bestString = fst . maximumBy (comparing snd) . fmap (identity &&& rankString)

hammingDistance :: Bits e => e -> e -> Int
hammingDistance e = popCount . xor e
