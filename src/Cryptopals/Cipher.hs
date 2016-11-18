{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Cipher where

import Protolude

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import qualified Codec.Crypto.AES as AES

import Cryptopals.Encoding
import Cryptopals.Util

allBytes, printableBytes :: [Char]
allBytes       = ['\x00'..'\xff']
printableBytes = ['\x20'..'\x7e']

type Key = ByteString
type KeySpace = [Key]

type Cipher e = Key -> e -> e
newtype CipherText a = CipherText a
newtype PlainText a = PlainText a
type Encipher e = Key -> PlainText e -> CipherText e
type Decipher e = Key -> CipherText e -> PlainText e

keySpace :: Integral n => n -> KeySpace
keySpace n = fmap S.pack . sequence . replicate (fromIntegral n) $ allBytes

xorCipher :: Encoding e ByteString => Cipher (e ByteString)
xorCipher k' e = onRaw (xor k) e
    where l = fromIntegral . S.length . toRaw $ e
          k = L.toStrict . L.take l . L.cycle . L.fromStrict $ k'

aes128EncryptECB :: (Encoding e ByteString)
                 => Cipher (e ByteString)
aes128EncryptECB k = onRaw
    (AES.crypt' AES.ECB k (S.replicate 16 '\x00') AES.Encrypt)

aes128DecryptECB :: (Encoding e ByteString) => Cipher (e ByteString)
aes128DecryptECB k = onRaw
    (AES.crypt' AES.ECB k (S.replicate 16 '\x00') AES.Decrypt)

pkcs7 :: (Encoding e ByteString) => Int -> e ByteString -> e ByteString
pkcs7 n = onRaw pkcs7BS
    where pkcs7BS b = let l = n - (S.length b `rem` n)
            in S.append b $ S.replicate l (chr l)
