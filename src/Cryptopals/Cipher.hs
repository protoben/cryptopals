{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cryptopals.Cipher where

import Protolude

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import Crypto.Cipher.Types (cipherInit, ecbEncrypt, ecbDecrypt)
import Crypto.Cipher.AES (AES128, AES192, AES256)
import Crypto.Error (CryptoFailable(..))
import System.Random (Random, random, randomR)
import qualified Crypto.Cipher.Types as Crypto

import Cryptopals.Encoding
import Cryptopals.Util

allBytes, printableBytes :: [Char]
allBytes       = ['\x00'..'\xff']
printableBytes = ['\x20'..'\x7e']

type Key = ByteString
type KeySpace = [Key]

keySpace :: Integral n => n -> KeySpace
keySpace n = fmap S.pack . sequence . replicate (fromIntegral n) $ allBytes

data BlockSize = B128 | B256 deriving (Read, Show, Eq)
instance Enum BlockSize where
    fromEnum b | b == B128 = 0 | b == B256 = 1
    toEnum   n | n == 0 = B128 | n == 1 = B256
instance Random BlockSize where
    randomR r = first toEnum . randomR (bimap fromEnum fromEnum r)
    random    = randomR (B128,B256)

data IV = IV BlockSize ByteString deriving (Read, Show, Eq)

iv :: Raw b => BlockSize -> b -> IV
iv bs s | S.length raw > bytes = error . toS $ "iv is too big for " ++ show bs
        | otherwise            = IV bs $ pkcs7BS bytes raw
    where
    bytes = blockBytes bs
    raw   = toByteString s

zeroIv :: BlockSize -> IV
zeroIv bs = IV bs $ S.replicate (blockBytes bs) '\x00'

encodeIv :: Encoding e b => IV -> e b
encodeIv (IV _ b) = fromByteString b

data Mode = ECB | CBC IV deriving (Read, Show, Eq)

blockBytes :: Integral n => BlockSize -> n
blockBytes B128 = 16
blockBytes B256 = 32

type CipherFunction e = Key -> e -> e

data Cipher e = Cipher
    { encrypt :: Encoding e ByteString => CipherFunction (e ByteString)
    , decrypt :: Encoding e ByteString => CipherFunction (e ByteString)
    }

xorCipher :: Encoding e ByteString => Cipher e
xorCipher = Cipher f f
    where
    f key e = onRaw (xor $ pad key e) e
    rawLen  = fromIntegral . S.length . toRaw
    pad k e = L.toStrict . L.take (rawLen e) . L.cycle $ L.fromStrict k

aes :: Encoding e ByteString => Mode -> BlockSize -> Cipher e
aes ECB bs = Cipher{..}
    where
    encrypt = doAES ecbEncrypt bs
    decrypt = doAES ecbDecrypt bs
aes (CBC iv) bs = Cipher{..}
    where
    bytes       = blockBytes bs
    encrypt k x = fromRaw . S.concat. fmap toRaw . tailSafe $ pipeline
        where blocks   = chunkEnc bytes . pkcs7 bytes $ x
              pipeline = encodeIv iv :
                       [ doAES ecbEncrypt bs k (a -^- b)
                       | a <- blocks
                       | b <- pipeline
                       ]
    decrypt k x = fromRaw . S.concat . fmap (toRaw . fst) . tailSafe $ pipeline
        where blocks   = chunkEnc 16 . pkcs7 16 $ x
              pipeline = (undefined, encodeIv iv) :
                       [ (doAES ecbDecrypt bs k a -^- b, a)
                       | a     <- blocks
                       | (_,b) <- pipeline
                       ]

doAES :: Encoding e ByteString
      => (forall c. Crypto.BlockCipher c => c -> ByteString -> ByteString)
      -> BlockSize -> CipherFunction (e ByteString)
doAES f bs k = crypt . pkcs7 bytes
    where
    key   :: Crypto.BlockCipher bc => bc
    key   = assumeSuccess . cipherInit . S.take bytes $ pkcs7BS bytes k
    bytes = blockBytes bs
    crypt = onRaw $ case bs of
        B128 -> f (key::AES128)
        B256 -> f (key::AES256)

pkcs7BS :: Int -> ByteString -> ByteString
pkcs7BS n b = let l = (n - (S.length b `rem` n)) `rem` n
    in S.append b $ S.replicate l (chr l)

pkcs7 :: (Encoding e ByteString) => Int -> e ByteString -> e ByteString
pkcs7 n = onRaw $ pkcs7BS n
