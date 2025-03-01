{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void, forM)
import qualified Data.Text as T

rawInputData = "10\n20\n30"

main :: IO ()
main = do
  let rows = T.splitOn "\n" rawInputData
      nums = map (extractNumber . T.unpack) rows

  forM nums (\num -> putStrLn $ show num)
  return ()

extractNumber s = read s :: Int
