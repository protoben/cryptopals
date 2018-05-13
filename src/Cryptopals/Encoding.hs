{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cryptopals.Encoding where

import Protolude

import Data.String (IsString(..))
import qualified Data.ByteString as W8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import Network.Curl (URLString, curlGetString_)
import Data.Bits.ByteString ()

import Cryptopals.Util

class Encoding e where
    toRaw :: e -> ByteString
    fromRaw :: ByteString -> e
    onRaw :: (ByteString -> ByteString) -> e -> e
    onRaw f = fromRaw . f . toRaw
    opRaw :: (ByteString -> ByteString -> ByteString) -> e -> e -> e
    opRaw op e f = fromRaw $ toRaw e `op` toRaw f
    apRaw :: (ByteString -> a) -> e -> a
    apRaw f = f . toRaw
    convert :: Encoding f => e -> f
    convert = fromRaw . toRaw
    {-# MINIMAL toRaw, fromRaw #-}
instance Encoding [Char] where
    toRaw = S.pack
    fromRaw = S.unpack
instance Encoding [Word8] where
    toRaw = W8.pack
    fromRaw = W8.unpack
instance Encoding Text where
    toRaw = encodeUtf8
    fromRaw = decodeUtf8

infixl 7 -&-
(-&-) :: Encoding e => e -> e -> e
(-&-) = opRaw (.&.)

infixl 5 -|-
(-|-) :: Encoding e => e -> e -> e
(-|-) = opRaw (.|.)

infixl 6 -^-
(-^-) :: Encoding e => e -> e -> e
(-^-) = opRaw xor

oneBits :: (Encoding e, Integral n) => e -> n
oneBits = fromIntegral . popCount . toRaw

bitSize :: (Encoding e, Integral n) => e -> n
bitSize = fromIntegral . finiteBitSize . toRaw

newtype Ascii = Ascii ByteString deriving (Read, Show, Eq)
instance Encoding Ascii where
    toRaw (Ascii bs) = bs
    fromRaw = Ascii
instance Monoid Ascii where
    mempty = fromRaw mempty
    mappend = opRaw mappend
instance IsString Ascii where
    fromString = convert

newtype Hex = Hex ByteString deriving (Read, Show, Eq)
instance Encoding Hex where
    toRaw (Hex bs) = fst . B16.decode $ bs
    fromRaw = Hex . B16.encode
instance Monoid Hex where
    mempty = fromRaw mempty
    mappend = opRaw mappend
instance IsString Hex where
    fromString = convert . ascii

newtype Base64 = Base64 ByteString deriving (Read, Show, Eq)
instance Encoding Base64 where
    toRaw (Base64 bs) = B64.decodeLenient bs
    fromRaw = Base64 . B64.encode
instance Monoid Base64 where
    mempty = fromRaw mempty
    mappend = opRaw mappend
instance IsString Base64 where
    fromString = convert . ascii

ascii :: Encoding e => e -> Ascii
ascii = convert

hex :: Encoding e => e -> Hex
hex = convert

base64 :: Encoding e => e -> Base64
base64 = convert

string :: Encoding e => e -> [Char]
string = convert

word8 :: Encoding e => e -> [Word8]
word8 = convert

text :: Encoding e => e -> Text
text = convert

chunkEnc :: (Encoding e, Integral n) => n -> e -> [e]
chunkEnc n = fmap fromRaw . chunkBS n . toRaw

chunkZip :: Encoding e => [e] -> e
chunkZip = fromRaw . chunkZipBS . fmap toRaw

chunkUnzip :: (Encoding e, Integral n) => n -> e -> [e]
chunkUnzip n = fmap fromRaw . chunkUnzipBS n . toRaw

readBase64File :: FilePath -> IO Base64
readBase64File = fmap (Base64 . S.concat . S.lines) . S.readFile

readBase64Lines :: FilePath -> IO [Base64]
readBase64Lines = fmap (fmap Base64 . S.lines) . S.readFile

readHexLines :: FilePath -> IO [Hex]
readHexLines = fmap (fmap Hex . S.lines) . S.readFile

curlBase64File :: URLString -> IO Base64
curlBase64File = fmap (Base64 . S.concat . S.lines . snd) . flip curlGetString_ []

curlBase64Lines :: URLString -> IO [Base64]
curlBase64Lines = fmap (fmap Base64 . S.lines . snd) . flip curlGetString_ []

curlHexLines :: URLString -> IO [Hex]
curlHexLines = fmap (fmap Hex . S.lines . snd) . flip curlGetString_ []
