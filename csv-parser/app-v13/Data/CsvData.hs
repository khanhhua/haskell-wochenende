module Data.CsvData where

data CsvType = Text | Number
data CsvData = CsvText String | CsvNumber Int
  deriving Show

