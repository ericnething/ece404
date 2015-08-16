{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.BitVector as BV
import           Data.BitVector (BitVector)
import Data.Bits (xor)
import Data.Char (ord, chr, isDigit, toLower)
import Data.List (foldl', foldl1')

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Word (Word8)

import Data.Monoid ((<>))

import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T (unlines)
import           Data.Text (Text)
import qualified Data.Text.IO as T (putStrLn, putStr)

import System.Environment (getArgs)
import Data.Maybe (catMaybes)

blockSize :: Int
blockSize = 16

numBytes :: Int
numBytes = blockSize `div` 8

passphrase = "Hopes and dreams of a million years"

targetCiphertext :: String
--targetCiphertext = "20352a7e36703a6930767f7276397e376528632d6b6665656f6f6424623c2d30272f3c2d3d2172396933742c7e233f687d2e32083c11385a03460d440c25"
targetCiphertext = "033f6479176d602b1d216b6b596b2a79507a303c55332b39503e2f58"

byteStringToBV :: ByteString -> BitVector
byteStringToBV = BS.foldl' g (BV.zeros 0)
  where
    g :: BitVector -> Word8 -> BitVector
    g acc c = acc BV.# BV.bitVec 8 (fromIntegral c)

keyToBV :: ByteString -> BitVector
keyToBV = foldl' g (BV.bitVec blockSize 0) . splitBS
  where g acc str = acc `xor` byteStringToBV str

splitBS :: ByteString -> [ByteString]
splitBS = go []
  where
    go :: [ByteString] -> ByteString -> [ByteString]
    go xs str
      | BS.null str = xs
      | otherwise = let (chunk, bs) = BS.splitAt numBytes (pad str)
                    in go (xs ++ [chunk]) bs

    pad str
      | BS.length str < numBytes = str <> BS.replicate (numBytes - BS.length str) 0
      | otherwise = str

encrypt :: BitVector -> ByteString -> BitVector
encrypt k = snd . foldl' g (keyToBV passphrase, BV.zeros 0)
            . map byteStringToBV . splitBS
  where
    g :: (BitVector, BitVector) -> BitVector -> (BitVector, BitVector)
    g (prev, acc) x = let prev' = k `xor` prev `xor` x
                      in (prev', acc BV.# prev')

hexStringToBV :: String -> BitVector
hexStringToBV = foldl1' BV.cat . map (BV.bitVec 4 . readHex)

readHex :: Char -> Integer
readHex c
  | isDigit c = read [c]
  | otherwise =
      case toLower c of
       'a' -> 10
       'b' -> 11
       'c' -> 12
       'd' -> 13
       'e' -> 14
       'f' -> 15
       _   -> error "Invalid character: must be hexadecimal"

splitString :: String -> [String]
splitString = go []
  where
    go :: [String] -> String -> [String]
    go xs [] = xs
    go xs str = let (chunk, s) = splitAt (numBytes*2) (pad str)
                in go (xs ++ [chunk]) s
    pad str
      | length str < (numBytes*2) = str ++ replicate (numBytes*2 - length str) '0'
      | otherwise = str

bvToByteString :: BitVector -> ByteString
bvToByteString bv
  | size > 0 = byte `BS.cons` bvToByteString rest
  | otherwise = BS.empty
  where byte = fromIntegral . BV.nat . BV.most chunk $ bv
        rest | size - chunk > 0 = BV.least (size - chunk) bv -- (max (size - 1 - chunk) 0)
             | otherwise = BV.zeros 0
        size = BV.size bv
        chunk = 8 -- min 8 size

decrypt :: BitVector -> String -> ByteString
decrypt k = bvToByteString . snd . foldl' g (keyToBV passphrase, BV.zeros 0)
            . map hexStringToBV . splitString
  where
    g :: (BitVector, BitVector) -> BitVector -> (BitVector, BitVector)
    g (prev, acc) x = let prev' = k `xor` prev `xor` x
                      in (x, acc BV.# prev')

-- | Generate 2^bits keys
generateKeys :: Int -> [BitVector]
generateKeys bits = map (BV.bitVec bits) [0..2^bits]

-- | Mount attack on target ciphertext with a key, producing the key
-- if it succeeds
attack :: String -> BitVector -> Maybe (BitVector, ByteString)
attack target key
  | check plaintext = Just (key, plaintext)
  | otherwise       = Nothing
  where plaintext = decrypt key target
        check p = any (`BS.isInfixOf` p) candidates
        candidates = [ " and ", " the ", " or ", " is "
                     , " for ", " when ", " are ", " of"
                     ]

bruteForce :: String -> [BitVector] -> [(BitVector, ByteString)]
bruteForce target keys = catMaybes $ map (attack target) keys

showResults :: (BitVector, ByteString) -> Text
showResults (k, p) = T.unlines [ "Key: " <> decodeUtf8 (bvToByteString k)
                               , "Plaintext: " <> decodeUtf8 p
                               ]

main :: IO ()
main = do
  putStrLn $ "Mounting brute force attack on: " <> targetCiphertext
  putStrLn $ "Block size set to " <> show blockSize

  let candidates = bruteForce targetCiphertext (generateKeys blockSize)
  case candidates of
   [] -> putStrLn "Attack Failed"
   xs -> do
     putStrLn "Success!"
     mapM_ (T.putStr . showResults) candidates
