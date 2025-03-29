module Main where

import System.IO (IOMode(..), openFile, hGetContents)

main :: IO ()
main = do
    handle <- openFile "dates.csv" ReadMode
    str <- hGetContents handle
    putStrLn str
