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
      extractFn = extractCsvData <$> extractName <*> extractLevel
      progLangRows = map (fromCsv . extractFn . extractRow) rows :: [ProgLang]
      -- TODO: explain the above context free notation
      -- progLangRows = map fromCsv (map extractFn $ map extractRow rows) :: [ProgLang]

  forM progLangRows (\row -> putStrLn $ show row)
  return ()

extractRow text = convert $ T.splitOn ";" text
 where
  convert (name : level : []) =
    [CsvText $ T.unpack name, CsvNumber (read $ T.unpack level :: Int)]

extractCsvData :: String -> Int -> [CsvData]
extractCsvData name level = [CsvText name, CsvNumber level]

extractName :: [CsvData] -> String
extractName (item : _items) =
  case item of
    CsvText s -> s
    _ -> error "Invalid row data"

extractLevel :: [CsvData] -> Int
extractLevel (_item : item : _items) =
  case item of
    CsvNumber n -> n
    _ -> error "Invalid row data"

