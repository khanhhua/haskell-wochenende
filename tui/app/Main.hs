module Main where
-- The ANSI escape sequence
-- https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
main :: IO ()
main = do
  -- putStrLn "\x1b[0;0HHello World!"
  -- putStrLn "\x1b[10;5H\x1b[1;35mHello, \x1b[1;31mHaskell!"

  putStrLn $ text (20, 8) (-3, 2) "Welcome to Haskell Wochenende"
  putStrLn $ text (20, 8) (3, 5) "Welcome to Haskell Wochenende"
  putStrLn $ border 20 8

border :: Int -> Int -> String
border width height =
  "\x1b[0;0f" <>
  "┌" <> (replicate (width - 2) '─') <> "┐\x1b[2;0f" <>
  drawBody <>
  "└" <> (replicate (width - 2) '─') <> "┘"
 where
  drawRow line =
    "│\x1b[" <> (show line) <> ";" <> (show width) <> "f│\x1b[" <> (show $ line + 1) <> ";0f"

  drawBody =
    concat [drawRow line | line <- [2..(height - 1)]]

text :: (Int, Int) -> (Int, Int) -> String -> String
text (width, height) (x, y) str =
  if x >= width || y >= height
    then mempty
    else 
      "\x1b[" <> (show y) <> ";" <> (show cappedX) <> "f" <> truncatedStr
 where
  cappedX = max x 0
  droppedPrefix = if x < 0 then abs x else 0
  truncatedStr = take (width - cappedX) $ drop droppedPrefix str

