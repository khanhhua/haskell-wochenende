{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void, forM)
import qualified Data.Text as T

data CsvType = Text | Number
data CsvData = CsvText String | CsvNumber Int
  deriving Show

rawInputData = "10\n20\n30"

main :: IO ()
main = do
  let rows = T.splitOn "\n" rawInputData
      nums = map (extractData Number . T.unpack) rows

  forM nums (\num -> putStrLn $ show num)
  return ()

extractData csvType s =
  case csvType of
    Text -> CsvText s
    Number -> CsvNumber $ extractNumber s

extractNumber s = read s :: Int

