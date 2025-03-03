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


runExtractConfig config csvData =
  let f = mapperFn config
  in f csvData

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
      extractFn = extractCsvData <$> mapName <*> mapLevel
      
      config = ExtractConfig
        { convertors = [ convertText Text
                       , convertText Number
                       ]
        , mapperFn = undefined
        }
      progLangRows = map (fromCsv . extractFn . extractRow config) rows :: [ProgLang]
      -- TODO: explain the above context free notation
      -- progLangRows = map fromCsv (map extractFn $ map extractRow rows) :: [ProgLang]

  forM progLangRows (\row -> putStrLn $ show row)
  return ()

extractRow :: ExtractConfig ProgLang -> T.Text  -> [CsvData]
extractRow config text =
  let values = T.splitOn ";" text
      tupleValueTypes = zip (convertors config) values
  in map (\(fn, value) -> (fn value)) tupleValueTypes

convertText Text text = CsvText $ T.unpack text
convertText Number text = CsvNumber (read $ T.unpack text :: Int)

extractCsvData :: String -> Int -> [CsvData]
extractCsvData name level = [CsvText name, CsvNumber level]

mapName :: [CsvData] -> String
mapName (item : _items) =
  case item of
    CsvText s -> s
    _ -> error "Invalid row data"

mapLevel :: [CsvData] -> Int
mapLevel (_item : item : _items) =
  case item of
    CsvNumber n -> n
    _ -> error "Invalid row data"

