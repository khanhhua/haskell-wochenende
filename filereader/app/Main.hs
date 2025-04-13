module Main where

import qualified Data.Char as Char
import System.IO (IOMode(..), openFile, hGetContents)

split  :: String -> [String]
split s = case w of
    "" -> []
    _ -> w : split (dropComma s'')
 where
  (w, s'') = break isComma s
  isComma c =
    c == ','
  dropComma = dropWhile isComma

main :: IO ()
main = do
    handle <- openFile "dates.csv" ReadMode
    str <- hGetContents handle
    let (header:body) = lines str 
        (tomLine:rest) = body
        (jerryLine:[]) = rest
    print tomLine
    print jerryLine
  
    let [name, dob, origin] = split jerryLine
        figure = Figure name dob origin
    print figure
  --  putStrLn $ show figure

data Figure = Figure String String String

instance Show Figure where
  show (Figure name _ origin) = name <> " (" <> origin <> ")"