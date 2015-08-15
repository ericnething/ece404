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
import qualified Data.Text.IO as T (putStrLn)
import System.Environment (getArgs)

blockSize :: Int
blockSize = 64

numBytes :: Int
numBytes = blockSize `div` 8

passphrase = "Hopes and dreams of a million years"

testSplitBS :: IO ()
testSplitBS = do
  putStr "ByteString: "
  bs <- BS.getLine 
  putStrLn $ "It has length of " <> show (BS.length bs) <> " bytes"
  let newBS = BS.concat $ splitBS bs
  putStrLn $ "Now it has length of " <> show (BS.length bs) <> " bytes"
  T.putStrLn $ "Plaintext: " <> decodeUtf8 newBS

testBStoBV :: IO ()
testBStoBV = do
  putStr "ByteString: "
  bs <- BS.getLine
  putStrLn $ "It has length of " <> show (BS.length bs) <> " bytes"
  let bv = byteStringToBV bs
  putStrLn $ "BitVector (bin): " <> BV.showBin bv
  putStrLn $ "BitVector (hex): " <> BV.showHex bv

  putStrLn "BitVector -> ByteString:"
  T.putStrLn $ "Plaintext: " <> decodeUtf8 (bvToByteString bv)

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

testEncrypt :: IO ()
testEncrypt = do
  putStr "Key: "
  key <- fmap keyToBV BS.getLine
  
  putStr "Plaintext: "
  plaintext <- BS.getLine

  putStrLn "Encrypting..."
  let ciphertext = encrypt key plaintext

  putStrLn $ "Ciphertext: " <> BV.showHex ciphertext

testEncrypt2 :: IO ()
testEncrypt2 = do
  -- putStr "Key: "
  -- key <- fmap keyToBV BS.getLine
  let key = keyToBV "hello"
  
  putStrLn "Reading File..."
  [file] <- getArgs
  plaintext <- BS.readFile file
  
  putStrLn "Encrypting..."
  let ciphertext = encrypt key plaintext

  putStrLn $ "Ciphertext: " <> BV.showHex ciphertext

testDecrypt :: IO ()
testDecrypt = do
  putStr "Key: "
  key <- fmap keyToBV BS.getLine
  
  putStr "Ciphertext: "
  ciphertext <- getLine

  putStrLn "Decrypting..."
  let plaintext = decrypt key ciphertext

  T.putStrLn $ "Plaintext: " <> decodeUtf8 plaintext
  --BS.writeFile "decrypted.txt" plaintext

main :: IO ()
main = testEncrypt2
