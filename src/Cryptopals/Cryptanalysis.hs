{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cryptopals.Cryptanalysis where

import Protolude

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString as B
import Control.Arrow ((&&&))
import System.Random (RandomGen, random, randomR)

import Cryptopals.Encoding
import Cryptopals.Statistics
import Cryptopals.Cipher
import Cryptopals.Util

rankKeys :: Encoding e ByteString
         => Cipher e -> KeySpace -> e ByteString
         -> [(Key, e ByteString)]
rankKeys c ks ct = fmap fst . reverse . sortOn snd . enc $ ks
    where
    enc = fmap (identity &&& rankString . snd) . fmap (identity &&& c' ct)
    c'  = flip $ encrypt c

rankXorKeys :: (Encoding e ByteString, Integral n)
            => n -> e ByteString -> [(Key, e ByteString)]
rankXorKeys n = fmap (rejoin . catPairs) . transpose . fmap decipher . chunkUnzip n
    where
    rejoin   = first S.concat . second chunkZip
    catPairs = foldr (\(a,b) (l,m) -> (a:l,b:m)) ([],[])
    decipher = rankKeys xorCipher (keySpace 1)

rankXorKeySize :: Encoding e ByteString => e ByteString -> [(Int,Double)]
rankXorKeySize s' = sortOn snd . fmap (identity &&& norm dist) $ [2..S.length s `div` 2]
    where
    dist   = averageIntegrals . mapPairwise hammingDistance . flip chunkDropBS s
    norm f = uncurry (/) . (f &&& fromIntegral)
    s      = toRaw s'

bestXorKeySize :: (Integral n, Encoding e ByteString) => n -> e ByteString -> Int
bestXorKeySize n = foldr gcd 0 . fmap fst . take (fromIntegral n) . rankXorKeySize

bruteforceNaive :: Encoding e ByteString
                => Cipher e -> KeySpace -> e ByteString
                -> Maybe (Key, e ByteString)
bruteforceNaive c ks = headMay . rankKeys c ks

bruteforceXor :: (Encoding e ByteString, Integral n)
              => n -> e ByteString -> Maybe (Key, e ByteString)
bruteforceXor n = headMay . rankXorKeys n

bestBruteforceXor :: Encoding e ByteString => [e ByteString] -> (Key, e ByteString)
bestBruteforceXor = bestStringBy snd . catMaybes . fmap (bruteforceXor 1)

possibly128ECB :: Encoding e ByteString => e ByteString -> Bool
possibly128ECB = or . fmap (uncurry (==)) . choose2 . enumerate . toBlocks
    where
    toBlocks  = chunkDropBS 16 . toRaw
    choose2 l = [(snd a,snd b) | a <- l, b <- l, fst a /= fst b]
    enumerate = zip [(1::Int)..]

findPossible128ECB :: Encoding e ByteString => [e ByteString] -> [e ByteString]
findPossible128ECB = fmap snd . filter fst . fmap (possibly128ECB &&& identity)

randomByteString :: (Integral n, RandomGen g) => n -> g -> (ByteString, g)
randomByteString n g = (B.pack (fmap fst nrands), lastDef g (fmap snd nrands))
    where
    rands  = random g : [random g' | (_,g') <- rands]
    nrands = take (fromIntegral n) rands

randomAesKey :: RandomGen g => BlockSize -> g -> (Key, g)
randomAesKey bs = randomByteString $ blockBytes bs

randomIV :: RandomGen g => BlockSize -> g -> (IV, g)
randomIV bs = first (IV bs) . randomByteString (blockBytes bs)

randomMode :: RandomGen g => BlockSize -> g -> (Mode, g)
randomMode bs g = (,g2) $ if b then CBC iv else ECB
    where
    (b,g1)  = random g
    (iv,g2) = if b then randomIV bs g1 else (undefined,g1)

randomBlockSize :: RandomGen g => g -> (BlockSize, g)
randomBlockSize = first (bool B256 B128) . random

type EncryptionOracle e = e -> e

randomAesOracle :: (RandomGen g, Encoding e ByteString)
                => Maybe Mode
                -> Maybe BlockSize
                -> Maybe Key
                -> Maybe (e ByteString)
                -> Maybe (e ByteString)
                -> g -> EncryptionOracle (e ByteString)
randomAesOracle mode' bs' key' prefix' suffix' g e =
    encrypt (aes mode bs) key $ fromByteString (prefix <> toByteString e <> suffix)
    where
    (bs,g1)   = maybe (randomBlockSize g)  (,g)  bs'
    (mode,g2) = maybe (randomMode bs g1)   (,g1) mode'
    (key,g3)  = maybe (randomAesKey bs g2) (,g2) key'
    prefix    = maybe mempty toByteString prefix'
    suffix    = maybe mempty toByteString suffix'

isProbablyCBC :: Encoding e ByteString
              => BlockSize -> EncryptionOracle (e ByteString) -> Bool
isProbablyCBC bs enc = det . take 2 . drop 1 . chunkEnc bbs $ ct
    where
    ct  = enc (convert . Ascii $ S.replicate (3*bbs) 'A')
    bbs = blockBytes bs
    det = \[e,f] -> toRaw e /= toRaw f

findBlockSize :: Encoding e ByteString
              => EncryptionOracle (e ByteString) -> Maybe BlockSize
findBlockSize o = msum $ blockSize . ctSizeDiff <$> [1..]
    where
    ctSize     n = apByteString S.length . o . convert . Ascii $ S.replicate n 'A'
    ctSizeDiff n = ctSize n - ctSize 0
    blockSize 16 = Just B128
    blockSize 32 = Just B256
    blockSize  _ = Nothing

