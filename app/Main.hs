module Main where

import qualified Data.Text as T
import Patterns 

p1 = Until (Char ' ')
p2 = Sequence (Match $ Char '{') (Match $ Char '}')
patterns = [p1,p2]

main :: IO ()
main =  do
    line <- getLine
    let parsed = parseText (T.pack line) patterns
    putStrLn $ T.unpack (T.unlines parsed)
