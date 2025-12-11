{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import System.Directory

data Student = Student
  { sid :: !Int
  , name :: !Text
  , age :: !Int
  } deriving Show

data Grade = Grade
  { sid_g :: !Int
  , course :: !Text
  , grade :: !Int
  } deriving Show

instance FromNamedRecord Student where
  parseNamedRecord m =
    Student <$> m .: "student_id"
            <*> m .: "name"
            <*> m .: "age"

instance FromNamedRecord Grade where
  parseNamedRecord m =
    Grade <$> m .: "student_id"
          <*> m .: "course"
          <*> m .: "grade"

main :: IO ()
main = do
  sData <- BL.readFile "bronze/student.csv"
  gData <- BL.readFile "bronze/grades.csv"

  let Right (_, students) = decodeByName sData :: Either String (_, V.Vector Student)
  let Right (_, grades)   = decodeByName gData :: Either String (_, V.Vector Grade)

  -- SILVER join
  let cleanedS = V.filter (\st -> age st > 0) students
  let cleanedG = V.filter (\gr -> grade gr > 0) grades

  let unified =
        [ (sid st, name st, age st, course gr, grade gr)
        | st <- V.toList cleanedS
        , gr <- V.toList cleanedG
        , sid st == sid_g gr
        ]

  createDirectoryIfMissing True "silver"
  writeFile "silver/unified.csv" $
    "student_id,name,age,course,grade\n"
    ++ unlines [ show a ++ "," ++ unpack b ++ "," ++ show c ++ "," ++ unpack d ++ "," ++ show e
               | (a,b,c,d,e) <- unified
               ]

  -- GOLD – star schema
  createDirectoryIfMissing True "gold"
  writeFile "gold/star.csv" $
    "student_id,name,age,course,grade\n"
    ++ unlines [ show a ++ "," ++ unpack b ++ "," ++ show c ++ "," ++ unpack d ++ "," ++ show e
               | (a,b,c,d,e) <- unified
               ]

  -- Simple ASCII flowchart
  createDirectoryIfMissing True "plots"
  writeFile "plots/flowchart.txt" $
    unlines
      [ "+-----------+"
      , "| BRONZE    |"
      , "+-----------+"
      , "      |"
      , "      v"
      , "+-----------+"
      , "| SILVER    |"
      , "+-----------+"
      , "      |"
      , "      v"
      , "+-----------+"
      , "| GOLD      |"
      , "+-----------+"
      ]

  putStrLn "Pipeline ready. Check silver/, gold/, plots/."
