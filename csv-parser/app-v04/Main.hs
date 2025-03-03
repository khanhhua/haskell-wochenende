{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void, forM)
import qualified Data.Text as T

data CsvType = Text | Number
data CsvData = CsvText String | CsvNumber Int
  deriving Show

rawInputData = "Java;10\nPHP;20\nPascal;30"

main :: IO ()
main = do
  let rows = T.splitOn "\n" rawInputData
      csvTypes = [ Text, Number ]
      nums = map (extractRow csvTypes) rows

  forM nums (\num -> putStrLn $ show num)
  return ()


extractRow csvTypes row =
  let items = T.splitOn ";" row
      pairs = zip csvTypes items
  in map (\(csvType, s) -> extractData csvType $ T.unpack s) pairs

extractData csvType s =
  case csvType of
    Text -> CsvText s
    Number -> CsvNumber $ extractNumber s

extractNumber s = read s :: Int

