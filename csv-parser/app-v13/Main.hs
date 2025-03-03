{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM)
import qualified Data.Text as T

import Data.CsvData
import Data.ExtractConfig

data ProgLang = ProgLang String Int
  deriving Show

instance FromCsv ProgLang where
  fromCsv [CsvText name, CsvNumber level] = ProgLang name level
  fromCsv _ = error "Could not parse"


rawInputData = "Java;10\nPHP;20\nPascal;30"

main :: IO ()
main = do
  let rows = T.splitOn "\n" rawInputData
      nameConfig = config (convertText Text) mapName
      levelConfig = config (convertText Number) mapLevel
      progLangConfig =
        ProgLang
          <$> nameConfig
          <*> levelConfig :: ExtractConfig ProgLang

      progLangRows = map (extractRow progLangConfig) rows

  forM progLangRows (\row -> putStrLn $ show row)
  return ()

convertText Text text = CsvText $ T.unpack text
convertText Number text = CsvNumber (read $ T.unpack text :: Int)

-- TODO: Explain the relationship between mapName, mapLevel and ProgLang
-- in their relation with the "config" Applicative.
-- ProgLang is a product type https://en.wikipedia.org/wiki/Product_(category_theory)
-- - When config = (\csvName csvLevel -> fromCsv [csvName, csvLevel]) <$> ... <*> ...,
--   `mapName [CsvData] -> CsvData` and `mapLevel [CsvData] -> CsvData`
-- - When config = (\name level -> ProgLang name level) <$> ... <*> ...,
--   `mapName [CsvData] -> String` and `mapLevel [CsvData] -> Int`
mapName :: [CsvData] -> String
mapName (item : _items) = case item of
  CsvText text -> text
  _ -> error "Invalid entry"

mapLevel :: [CsvData] -> Int
mapLevel (_item : item : _items) = case item of
  CsvNumber num -> num
  _ -> error "Invalid entry"

