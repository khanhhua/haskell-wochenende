module Main where
-- The ANSI escape sequence
-- https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
main :: IO ()
main = do
  -- putStrLn "\x1b[0;0HHello World!"
  -- putStrLn "\x1b[10;5H\x1b[1;35mHello, \x1b[1;31mHaskell!"

  let t1 = Text (20, 8) (-3, 2) "Welcome to Haskell Wochenende"
      t2 = Text (20, 8) (3, 5) "Welcome to Haskell Wochenende"
      b1 = Border (20, 8)
  putStrLn $ concat [render element | element <- [t1, t2, b1]]

type Size = (Int, Int)
type Position = (Int, Int)

data TuiElement
  = Border Size
  | Text Size Position String


render :: TuiElement -> String
render (Border size) = border size
render (Text size pos str) = text size pos str


border :: (Int, Int) -> String
border (width, height) =
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

