{-# LANGUAGE FlexibleContexts #-}
module Cryptopals.Cryptanalysis where

import Protolude

import qualified Data.ByteString.Char8 as S
import Control.Arrow ((&&&))

import Cryptopals.Encoding
import Cryptopals.Statistics
import Cryptopals.Cipher
import Cryptopals.Util

rankKeys :: Encoding e S.ByteString
         => Cipher (e S.ByteString) -> KeySpace -> e S.ByteString
         -> [(Key, e S.ByteString)]
rankKeys c ks ct = fmap fst . reverse . sortOn snd . enc $ ks
    where enc = fmap (identity &&& rankString . snd) . fmap (identity &&& flip c ct)

rankXorKeys :: (Encoding e S.ByteString, Integral n)
            => n -> e S.ByteString -> [(Key, e S.ByteString)]
rankXorKeys n = fmap (rejoin . catPairs) . transpose . fmap decipher . chunkUnzip n
    where rejoin   = first S.concat . second chunkZip
          catPairs = foldr (\(a,b) (l,m) -> (a:l,b:m)) ([],[])
          decipher = rankKeys xorCipher (keySpace 1)

rankXorKeySize :: Encoding e S.ByteString => e S.ByteString -> [(Int,Double)]
rankXorKeySize s' = sortOn snd . fmap (identity &&& norm dist) $ [2..S.length s `div` 2]
    where dist   = averageIntegrals . mapPairwise hammingDistance . flip chunkDropBS s
          norm f = uncurry (/) . (f &&& fromIntegral)
          s      = toRaw s'

bestXorKeySize :: (Integral n, Encoding e S.ByteString) => n -> e S.ByteString -> Int
bestXorKeySize n = foldr gcd 0 . fmap fst . take (fromIntegral n) . rankXorKeySize

bruteforceNaive :: Encoding e S.ByteString
                => Cipher (e S.ByteString) -> KeySpace -> e S.ByteString
                -> Maybe (Key, e S.ByteString)
bruteforceNaive c ks = headMay . rankKeys c ks

bruteforceXor :: (Encoding e S.ByteString, Integral n)
              => n -> e S.ByteString -> Maybe (Key, e S.ByteString)
bruteforceXor n = headMay . rankXorKeys n

possiblyAes128ECB :: Encoding e ByteString => e ByteString -> Bool
possiblyAes128ECB = or . fmap (uncurry (==)) . choose2 . enumerate . toBlocks
    where toBlocks  = chunkDropBS 16 . toRaw
          choose2 l = [(snd a,snd b) | a <- l, b <- l, fst a /= fst b]
          enumerate = zip [(1::Int)..]

findPossibleAes128ECB :: Encoding e ByteString => [e ByteString] -> [e ByteString]
findPossibleAes128ECB = fmap snd . filter fst . fmap (possiblyAes128ECB &&& identity)
