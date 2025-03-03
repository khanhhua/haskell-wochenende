{-# LANGUAGE OverloadedStrings #-}
module Data.ExtractConfig where

import qualified Data.Text as T
import Data.CsvData

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

instance (Semigroup a) => Semigroup (ExtractConfig a) where
  (<>) (ExtractConfig c1 f) (ExtractConfig c2 g) =
    ExtractConfig (c1 <> c2) ((\a b -> a <> b) <$> f <*> g)

instance Applicative ExtractConfig where
  pure x = ExtractConfig {convertors=[], mapperFn=(\_ -> x)}
  (<*>) (ExtractConfig {convertors=c1, mapperFn=f}) (ExtractConfig {convertors=c2, mapperFn=g}) =
    ExtractConfig {convertors=c1 <> c2, mapperFn=mapperFn'}
   where
    mapperFn' csvData =
      let a = g csvData
          f' = f csvData
      in f' a

class FromCsv a where
  fromCsv :: [CsvData] -> a

extractRow :: ExtractConfig a -> T.Text -> a
extractRow config text =
  let values = T.splitOn ";" text
      tupleValueTypes = zip (convertors config) values
      csvData = map (\(fn, value) -> (fn value)) tupleValueTypes
  in mapperFn config csvData

config convertor mapperFn =
  ExtractConfig
    { convertors= [convertor]
    , mapperFn = mapperFn
    }
