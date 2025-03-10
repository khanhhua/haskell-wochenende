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
      -- TODO: Explain the distributivity (f [x] == [f x])
      canvas = b1 <> t1 <> t2
      -- which means: canvas = Canvas (20, 8) [t1, t2, b1]
      AnsiPhrase tokens = render canvas
  mapM_ (\token -> case token of
      Ctrl (x, y) -> putStr $ show token
      Ansi str -> putStr str
      ) tokens


data AnsiToken
  = Ctrl Position
  | Ansi String

ctrl :: Int -> Int -> AnsiToken
ctrl x y = Ctrl (x, y)

ansi :: String -> AnsiToken
ansi = Ansi

instance Show AnsiToken where
  show (Ctrl (x, y)) = "\x1b[" <> (show y) <> ";" <> (show x) <> "f"
  show (Ansi c) = show c


data AnsiPhrase = AnsiPhrase [AnsiToken]

instance Show AnsiPhrase where
  show (AnsiPhrase xs) = concat $ map show xs
instance Semigroup AnsiPhrase where
  (<>) (AnsiPhrase a) (AnsiPhrase b) = AnsiPhrase $ a <> b
instance Monoid AnsiPhrase where
  mempty = AnsiPhrase []

phrase :: Int -> Int -> String -> AnsiPhrase
phrase x y str = AnsiPhrase [(ctrl x y), (ansi str)]

repline :: Int -> Char -> AnsiPhrase
repline times c = AnsiPhrase $ map ansi (replicate times [c])


type Size = (Int, Int)
type Position = (Int, Int)

data TuiElement
  = Border Size
  | Text Size Position String
  | Canvas Size [TuiElement]

instance Semigroup TuiElement where
  (<>) a@(Border sizeA) b@(Border sizeB) = Canvas (mergeSize sizeA sizeB) [a, b]
  (<>) a@(Border sizeA) b@(Text sizeB _ _) = Canvas (mergeSize sizeA sizeB) [a, b]
  (<>) a@(Text sizeA _ _) b@(Text sizeB _ _) = Canvas (mergeSize sizeA sizeB) [a, b]
  (<>) a@(Text sizeA _ _) b@(Border sizeB) = Canvas (mergeSize sizeA sizeB) [a, b]
  (<>) (Canvas sizeA xs) (Canvas sizeB ys) = Canvas (mergeSize sizeA sizeB) $ xs <> ys
  (<>) (Canvas size xs) b = Canvas (mergeSize size $ tuiElementSize b) $ b:xs
  (<>) a (Canvas size xs) = Canvas (mergeSize size $ tuiElementSize a) $ a:xs

tuiElementSize (Border size) = size
tuiElementSize (Text size _ _) = size
tuiElementSize (Canvas size _) = size

mergeSize (w1,h1) (w2,h2) = (max w1 w2, max h1 h2)

instance Monoid TuiElement where
  mempty = Canvas (0,0) []


render :: TuiElement -> AnsiPhrase
render (Border size) = border size
render (Text size pos str) = text size pos str
render (Canvas size elements) =
  (mconcat$ map render elements) <> AnsiPhrase [Ctrl size]

border :: (Int, Int) -> AnsiPhrase
border (width, height) =
  (phrase 0 0 "┌") <> (repline (width - 2) '─') <> AnsiPhrase [ansi "┐"] <>
  drawBody <>
  (phrase 0 height "└") <> (repline (width - 2) '─') <> AnsiPhrase [ansi "┘"]
 where
  drawRow line =
    AnsiPhrase [ctrl 0 line, ansi "│", ctrl width line, ansi "│"]

  drawBody :: AnsiPhrase
  drawBody = mconcat $ map drawRow [2..(height - 1)]


text :: (Int, Int) -> (Int, Int) -> String -> AnsiPhrase
text (width, height) (x, y) str =
  if x >= width || y >= height
    then mempty
    else
      phrase cappedX y truncatedStr
 where
  cappedX = max x 0
  droppedPrefix = if x < 0 then abs x else 0
  truncatedStr = take (width - cappedX) $ drop droppedPrefix str

