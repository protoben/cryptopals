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

rankKeys :: Encoding e => Cipher e -> KeySpace -> e -> [(Key, e)]
rankKeys c ks ct = fmap fst . reverse . sortOn snd . enc $ ks
    where
    enc = fmap (identity &&& rankString . snd) . fmap (identity &&& c' ct)
    c'  = flip $ encrypt c

rankXorKeys :: (Encoding e, Integral n) => n -> e -> [(Key, e)]
rankXorKeys n = fmap (rejoin . catPairs) . transpose . fmap decipher . chunkUnzip n
    where
    rejoin   = first S.concat . second chunkZip
    catPairs = foldr (\(a,b) (l,m) -> (a:l,b:m)) ([],[])
    decipher = rankKeys xorCipher (keySpace 1)

rankXorKeySize :: Encoding e => e -> [(Int,Double)]
rankXorKeySize s' = sortOn snd . fmap (identity &&& norm dist) $ [2..S.length s `div` 2]
    where
    dist   = averageIntegrals . mapPairwise hammingDistance . flip chunkDropBS s
    norm f = uncurry (/) . (f &&& fromIntegral)
    s      = toRaw s'

bestXorKeySize :: (Integral n, Encoding e) => n -> e -> Int
bestXorKeySize n = foldr gcd 0 . fmap fst . take (fromIntegral n) . rankXorKeySize

bruteforceNaive :: Encoding e => Cipher e -> KeySpace -> e -> Maybe (Key, e)
bruteforceNaive c ks = headMay . rankKeys c ks

bruteforceXor :: (Encoding e, Integral n) => n -> e -> Maybe (Key, e)
bruteforceXor n = headMay . rankXorKeys n

bestBruteforceXor :: Encoding e => [e] -> (Key, e)
bestBruteforceXor = bestStringBy snd . catMaybes . fmap (bruteforceXor 1)

possibly128ECB :: Encoding e => e -> Bool
possibly128ECB = or . fmap (uncurry (==)) . choose2 . enumerate . toBlocks
    where
    toBlocks  = chunkDropBS 16 . toRaw
    choose2 l = [(snd a,snd b) | a <- l, b <- l, fst a /= fst b]
    enumerate = zip [(1::Int)..]

findPossible128ECB :: Encoding e => [e] -> [e]
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

randomAesOracle :: (RandomGen g, Encoding e)
                => Maybe Mode -> Maybe BlockSize -> Maybe Key -> Maybe e -> Maybe e
                -> g -> EncryptionOracle e
randomAesOracle mode' bs' key' prefix' suffix' g e =
    encrypt (aes mode bs) key $ fromRaw (prefix <> toRaw e <> suffix)
    where
    (bs,g1)   = maybe (randomBlockSize g)  (,g)  bs'
    (mode,g2) = maybe (randomMode bs g1)   (,g1) mode'
    (key,g3)  = maybe (randomAesKey bs g2) (,g2) key'
    prefix    = maybe mempty toRaw prefix'
    suffix    = maybe mempty toRaw suffix'

isProbablyCBC :: Encoding e => BlockSize -> EncryptionOracle e -> Bool
isProbablyCBC bs enc = det . take 2 . drop 1 . chunkEnc bbs $ ct
    where
    ct  = enc (convert . Ascii $ S.replicate (3*bbs) 'A')
    bbs = blockBytes bs
    det = \[e,f] -> toRaw e /= toRaw f

findBlockSize :: Encoding e => EncryptionOracle e -> Maybe BlockSize
findBlockSize o = msum $ blockSize . ctSizeDiff <$> [1..]
    where
    ctSize     n = apRaw S.length . o . convert . Ascii $ S.replicate n 'A'
    ctSizeDiff n = ctSize n - ctSize 0
    blockSize 16 = Just B128
    blockSize 32 = Just B256
    blockSize  _ = Nothing

bruteforceAesNextByte :: Encoding e => EncryptionOracle e -> e -> e
bruteforceAesNextByte o = undefined
