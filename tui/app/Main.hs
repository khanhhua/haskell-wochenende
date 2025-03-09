module Main where
-- The ANSI escape sequence
-- https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
main :: IO ()
main = do
  -- putStrLn "\x1b[0;0HHello World!"
  -- putStrLn "\x1b[10;5H\x1b[1;35mHello, \x1b[1;31mHaskell!"

  putStrLn $ border 10 8

border :: Int -> Int -> String
border width height =
  "┌" <> (replicate (width - 2) '─') <> "┐\x1b[2;0f" <>
  drawBody <>
  "└" <> (replicate (width - 2) '─') <> "┘"
 where
  drawRow line =
    "│\x1b[" <> (show line) <> ";" <> (show width) <> "f│\x1b[" <> (show $ line + 1) <> ";0f"

  drawBody =
    (concat [(drawRow line) | line <- [2..(height - 1)]])
