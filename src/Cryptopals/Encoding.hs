{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Encoding where

import Protolude

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import Data.Bits.ByteString ()
import Data.String (fromString)

import Cryptopals.Util

class Encoding e b where
    toRaw :: e b -> b
    fromRaw :: b -> e b
    onRaw :: (b -> b) -> e b -> e b
    onRaw f = fromRaw . f . toRaw
    opRaw :: (b -> b -> b) -> e b -> e b -> e b
    opRaw op e f = fromRaw $ toRaw e `op` toRaw f
    convert :: Encoding f b => e b -> f b
    convert = fromRaw . toRaw
    {-# MINIMAL toRaw, fromRaw #-}
instance (Encoding e ByteString) => Eq (e ByteString) where
    e == f = toRaw e == toRaw f
instance (Encoding e ByteString) => IsString (e ByteString) where
    fromString = fromRaw . S.pack
instance (Encoding e ByteString) => Bits (e ByteString) where
    (.&.) = opRaw (.&.)
    (.|.) = opRaw (.|.)
    xor = opRaw xor
    complement = onRaw complement
    rotate e n = onRaw (flip rotate n) e
    shift e n = onRaw (flip shift n) e
    bitSize = maybe 0 identity . bitSizeMaybe
    bitSizeMaybe = bitSizeMaybe . toRaw
    isSigned = isSigned . toRaw
    bit = fromRaw . bit
    popCount = popCount . toRaw
    testBit = testBit . toRaw
instance (Encoding e ByteString) => FiniteBits (e ByteString) where
    finiteBitSize = finiteBitSize . toRaw

data Ascii b = Ascii b
deriving instance Show b => Show (Ascii b)
deriving instance Read b => Read (Ascii b)
instance Encoding Ascii ByteString where
    toRaw (Ascii bs) = bs
    fromRaw = Ascii

data Hex b = Hex b
deriving instance Show b => Show (Hex b)
deriving instance Read b => Read (Hex b)
instance Encoding Hex ByteString where
    toRaw (Hex bs) = fst . B16.decode $ bs
    fromRaw = Hex . B16.encode

data Base64 b = Base64 b
deriving instance Show b => Show (Base64 b)
deriving instance Read b => Read (Base64 b)
instance Encoding Base64 ByteString where
    toRaw (Base64 bs) = B64.decodeLenient bs
    fromRaw = Base64 . B64.encode

ascii :: (Encoding e ByteString) => e ByteString -> Ascii ByteString
ascii = convert

hex :: (Encoding e ByteString) => e ByteString -> Hex ByteString
hex = convert

base64 :: (Encoding e ByteString) => e ByteString -> Base64 ByteString
base64 = convert

chunkEnc :: (Encoding e ByteString, Integral n) => n -> e ByteString -> [e ByteString]
chunkEnc n = fmap fromRaw . chunkBS n . toRaw

chunkZip :: Encoding e ByteString => [e ByteString] -> e ByteString
chunkZip = fromRaw . chunkZipBS . fmap toRaw

chunkUnzip :: (Encoding e ByteString, Integral n)
           => n -> e ByteString -> [e ByteString]
chunkUnzip n = fmap fromRaw . chunkUnzipBS n . toRaw

readBase64File :: FilePath -> IO (Base64 ByteString)
readBase64File = fmap (Base64 . S.concat . S.lines) . S.readFile

readBase64Lines :: FilePath -> IO [Base64 ByteString]
readBase64Lines = fmap (fmap Base64 . S.lines) . S.readFile
