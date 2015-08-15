module Main where

import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import Data.Char (ord, isDigit, toUpper)

type VigSquare = Vector (Vector Char)


vigsquare :: VigSquare
vigsquare = mkVigsquare alphabet

mkVigsquare :: [Char] -> VigSquare
mkVigsquare alpha = V.fromList $ map (V.fromList . rotate alpha) alpha
  where rotate (x:xs) c
          | c == x = x:xs
          | otherwise = rotate (xs ++ [x]) c

alphabet :: [Char]
alphabet = ['.',',','?'] ++ concatMap show [0..9] ++ ['a'..'z']

getLetter :: Char -> Char -> Char
getLetter row column = vigsquare ! (idx row) ! (idx column)
  where idx '.' = 0
        idx ',' = 1
        idx '?' = 2
        idx c | isDigit c = ord c - ord '0' + 3
              | otherwise = ord c - ord 'a' + 13

mkCipherKey :: String -> [Char]
mkCipherKey = cycle

main :: IO ()
main = do
  putStr "Encryption Key: "
  key <- fmap mkCipherKey getLine
  putStr "Plaintext: "
  plaintext <- getLine
  let ciphertext = zipWith (getLetter) key plaintext
  putStrLn $ show ciphertext
