module Main where

import Matcha.Parser as Parser

main :: IO ()
main = interact (show . Parser.run)
