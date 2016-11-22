{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cryptopals.Encoding where

import Protolude

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import Network.Curl (URLString, curlGetString_)
import Data.Bits.ByteString ()

import Cryptopals.Util

class Raw b where
    toByteString :: b -> ByteString
    fromByteString :: ByteString -> b
    onByteString :: (ByteString -> ByteString) -> b -> b
    onByteString f = fromByteString . f . toByteString
    opByteString :: (ByteString -> ByteString -> ByteString) -> b -> b -> b
    opByteString op e f = fromByteString $ toByteString e `op` toByteString f
    {-# MINIMAL toByteString, fromByteString #-}

instance Raw ByteString where
    toByteString = identity
    fromByteString = identity

class Raw b => Encoding e b where
    toRaw :: e b -> b
    fromRaw :: b -> e b
    onRaw :: (b -> b) -> e b -> e b
    onRaw f = fromRaw . f . toRaw
    opRaw :: (b -> b -> b) -> e b -> e b -> e b
    opRaw op e f = fromRaw $ toRaw e `op` toRaw f
    convert :: Encoding f b => e b -> f b
    convert = fromRaw . toRaw
    {-# MINIMAL toRaw, fromRaw #-}
instance (Encoding e b) => Raw (e b) where
    toByteString = toByteString . toRaw
    fromByteString = fromRaw . fromByteString

infixl 7 -&-
(-&-) :: Encoding e ByteString => e ByteString -> e ByteString -> e ByteString
(-&-) = opRaw (.&.)

infixl 5 -|-
(-|-) :: Encoding e ByteString => e ByteString -> e ByteString -> e ByteString
(-|-) = opRaw (.|.)

infixl 6 -^-
(-^-) :: Encoding e ByteString => e ByteString -> e ByteString -> e ByteString
(-^-) = opRaw xor

oneBits :: (Encoding e ByteString, Integral n) => e ByteString -> n
oneBits = fromIntegral . popCount . toByteString

data Ascii b = Ascii b
deriving instance Show b => Show (Ascii b)
deriving instance Read b => Read (Ascii b)
deriving instance Eq b => Eq (Ascii b)
instance Encoding Ascii ByteString where
    toRaw (Ascii bs) = bs
    fromRaw = Ascii

data Hex b = Hex b
deriving instance Show b => Show (Hex b)
deriving instance Read b => Read (Hex b)
deriving instance Eq b => Eq (Hex b)
instance Encoding Hex ByteString where
    toRaw (Hex bs) = fst . B16.decode $ bs
    fromRaw = Hex . B16.encode

data Base64 b = Base64 b
deriving instance Show b => Show (Base64 b)
deriving instance Read b => Read (Base64 b)
deriving instance Eq b => Eq (Base64 b)
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

curlBase64File :: URLString -> IO (Base64 ByteString)
curlBase64File = fmap (Base64 . S.concat . S.lines . snd) . flip curlGetString_ []

curlBase64Lines :: URLString -> IO [Base64 ByteString]
curlBase64Lines = fmap (fmap Base64 . S.lines . snd) . flip curlGetString_ []
