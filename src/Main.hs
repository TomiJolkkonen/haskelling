{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.List (maximumBy)
import Data.Ord (comparing)

data City = City
  { city :: !String
  , population :: !Int
  } deriving Show

instance FromNamedRecord City where
  parseNamedRecord m = City
    <$> m .: "city"
    <*> m .: "population"

main :: IO ()
main = do
  csvData <- BL.readFile "data/example.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> do
      let largest = maximumBy (comparing population) (V.toList v)
      putStrLn $ "Largest city: " ++ city largest ++ " with " ++ show (population largest)

      putStrLn "\nTransformed (population in thousands):"
      mapM_ (\c -> putStrLn (city c ++ ": " ++ show (fromIntegral (population c) / 1000))) v
