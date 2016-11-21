{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cryptopals.Util where

import Protolude

import qualified Data.ByteString.Char8 as S
import Data.Bits.ByteString ()

instance FiniteBits ByteString where
    finiteBitSize = maybe 0 identity . bitSizeMaybe

chunkBS :: Integral n => n -> ByteString -> [ByteString]
chunkBS n' = let n = fromIntegral n' in
    unfoldr $ \bs -> case bs of
        "" -> Nothing
        _  -> Just (S.take n bs, S.drop n bs)

chunkDropBS :: Integral n => n -> ByteString -> [ByteString]
chunkDropBS n s = chunkBS n (S.take m s)
    where m  = fromIntegral $ n' * (S.length s `div` n')
          n' = fromIntegral n

chunkZipBS :: [ByteString] -> ByteString
chunkZipBS = S.concat . S.transpose

chunkUnzipBS :: Integral n => n -> ByteString -> [ByteString]
chunkUnzipBS n = S.transpose . chunkBS n

mapPairwise :: (a -> a -> b) -> [a] -> [b]
mapPairwise f l = fmap (uncurry f) $ zip l (tailSafe l)

average :: Fractional f => [f] -> f
average l = sum l / (fromIntegral . length) l

averageIntegrals :: (Integral i, Fractional f) => [i] -> f
averageIntegrals = average . fmap fromIntegral
