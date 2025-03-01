{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void, forM)
import qualified Data.Text as T

main :: IO ()
main = do
  let rows = T.splitOn "\n" rawInputData
      rowsAsStr = map T.unpack rows
  forM rowsAsStr putStrLn
  return ()
  -- Alternatives:
  -- 1:
  -- void $ mapM (putStrLn <$> T.unpack) rows
  -- 2:
  -- void $forM rows (putStrLn <$> T.unpack)

rawInputData = "Java\nPHP\nErlang"

