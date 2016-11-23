{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cryptopals.Cryptanalysis where

import Protolude

import qualified Data.ByteString.Char8 as S
import Control.Arrow ((&&&))

import Cryptopals.Encoding
import Cryptopals.Statistics
import Cryptopals.Cipher
import Cryptopals.Util

rankKeys :: Encoding e ByteString
         => Cipher (e ByteString) -> KeySpace -> e ByteString
         -> [(Key, e ByteString)]
rankKeys c ks ct = fmap fst . reverse . sortOn snd . enc $ ks
    where enc = fmap (identity &&& rankString . snd) . fmap (identity &&& flip c ct)

rankXorKeys :: (Encoding e ByteString, Integral n)
            => n -> e ByteString -> [(Key, e ByteString)]
rankXorKeys n = fmap (rejoin . catPairs) . transpose . fmap decipher . chunkUnzip n
    where rejoin   = first S.concat . second chunkZip
          catPairs = foldr (\(a,b) (l,m) -> (a:l,b:m)) ([],[])
          decipher = rankKeys xorCipher (keySpace 1)

rankXorKeySize :: Encoding e ByteString => e ByteString -> [(Int,Double)]
rankXorKeySize s' = sortOn snd . fmap (identity &&& norm dist) $ [2..S.length s `div` 2]
    where dist   = averageIntegrals . mapPairwise hammingDistance . flip chunkDropBS s
          norm f = uncurry (/) . (f &&& fromIntegral)
          s      = toRaw s'

bestXorKeySize :: (Integral n, Encoding e ByteString) => n -> e ByteString -> Int
bestXorKeySize n = foldr gcd 0 . fmap fst . take (fromIntegral n) . rankXorKeySize

bruteforceNaive :: Encoding e ByteString
                => Cipher (e ByteString) -> KeySpace -> e ByteString
                -> Maybe (Key, e ByteString)
bruteforceNaive c ks = headMay . rankKeys c ks

bruteforceXor :: (Encoding e ByteString, Integral n)
              => n -> e ByteString -> Maybe (Key, e ByteString)
bruteforceXor n = headMay . rankXorKeys n


bestBruteforceXor :: Encoding e ByteString => [e ByteString] -> (Key, e ByteString)
bestBruteforceXor = bestStringBy snd . catMaybes . fmap (bruteforceXor 1)

possibly128ECB :: Encoding e ByteString => e ByteString -> Bool
possibly128ECB = or . fmap (uncurry (==)) . choose2 . enumerate . toBlocks
    where toBlocks  = chunkDropBS 16 . toRaw
          choose2 l = [(snd a,snd b) | a <- l, b <- l, fst a /= fst b]
          enumerate = zip [(1::Int)..]

findPossible128ECB :: Encoding e ByteString => [e ByteString] -> [e ByteString]
findPossible128ECB = fmap snd . filter fst . fmap (possibly128ECB &&& identity)
