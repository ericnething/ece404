module Main where

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import Data.Char
import Data.List (sort)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.IO as T (readFile)

main :: IO ()
main = putStr . show . histogram =<< T.readFile . head =<< getArgs

newtype Histogram = Histogram { unHistogram :: Map Char Int }

instance Show Histogram where
  show = unlines . map f . sort . Map.toList . unHistogram
    where f (c,n) = c : " : " ++ show n

histogram :: Text -> Histogram
histogram = Histogram . Text.foldl' f Map.empty
  where f acc c | isAlpha c = Map.insertWith (+) (toUpper c) 1 acc
                | otherwise = acc
