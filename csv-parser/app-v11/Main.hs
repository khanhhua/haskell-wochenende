{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM)
import qualified Data.Text as T

data CsvType = Text | Number
data CsvData = CsvText String | CsvNumber Int
  deriving Show

data ExtractConfig a = ExtractConfig
  { convertors :: [T.Text -> CsvData]
  , mapperFn :: [CsvData] -> a
  }

instance Functor ExtractConfig where
  -- fmap :: (a -> b) -> ExtractConfig a -> ExtractConfig b
  fmap f (ExtractConfig {convertors=convertors_, mapperFn=g}) =
    ExtractConfig
      { convertors=convertors_
      , mapperFn=mapperFn'
      }
   where
    mapperFn' csvData = f (g csvData)

instance Applicative ExtractConfig where
  pure x = ExtractConfig {convertors=[], mapperFn=(\_ -> x)}
  (<*>) (ExtractConfig {convertors=c1, mapperFn=f}) (ExtractConfig {convertors=c2, mapperFn=g}) =
    ExtractConfig {convertors=c1 <> c2, mapperFn=mapperFn'}
   where
    mapperFn' csvData =
      let a = g csvData
          f' = f csvData
      in f' a

extractRow :: ExtractConfig a -> T.Text -> a
extractRow config text =
  let values = T.splitOn ";" text
      tupleValueTypes = zip (convertors config) values
      csvData = map (\(fn, value) -> (fn value)) tupleValueTypes
  in mapperFn config csvData

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
      nameConfig = ExtractConfig {convertors=[convertText Text], mapperFn=mapName}
      levelConfig = ExtractConfig {convertors=[convertText Number], mapperFn=mapLevel}
      -- TODO: Explain the categorical bijective relation between
      --       extractFn = extractCsvData <$> mapName <*> mapLevel
      --   and ....
      config = (\csvName csvLevel -> fromCsv [csvName, csvLevel])
                 <$> nameConfig
                 <*> levelConfig :: ExtractConfig ProgLang

      progLangRows = map (extractRow config) rows

  forM progLangRows (\row -> putStrLn $ show row)
  return ()

convertText Text text = CsvText $ T.unpack text
convertText Number text = CsvNumber (read $ T.unpack text :: Int)

mapName :: [CsvData] -> CsvData
mapName (item : _items) = item

mapLevel :: [CsvData] -> CsvData
mapLevel (_item : item : _items) = item

