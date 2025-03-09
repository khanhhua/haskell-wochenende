module Main where
-- The ANSI escape sequence
-- https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
main :: IO ()
main = do
  putStrLn "\x1b[0;0HHello World!"
  putStrLn "\x1b[10;5H\x1b[1;35mHello, \x1b[1;31mHaskell!"
