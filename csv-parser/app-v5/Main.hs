{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM)
import qualified Data.Text as T

data CsvType = Text | Number
data CsvData = CsvText String | CsvNumber Int
  deriving Show

class FromCsv a where
  fromCsv :: [CsvData] -> a

data ProgLang = ProgLang String Int
  deriving Show

instance FromCsv ProgLang where
  fromCsv [CsvText name, CsvNumber level] = ProgLang name level
  fromCsv _ = error "Could not parse"


rawInputData = "Java;10\nPHP;20\nPascal;30"

main :: IO ()
main = do
  let rows = T.splitOn "\n" rawInputData
      csvTypes = [ Text, Number ]
      progLangRows = map (\row -> 
                             let csvItems = extractRow csvTypes row
                             in fromCsv csvItems) rows :: [ProgLang]

  forM progLangRows (\row -> putStrLn $ show row)
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

