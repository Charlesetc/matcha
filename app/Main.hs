module Main where

import Matcha.Parser as Parser

main :: IO ()
main = putStrLn . show $ Parser.run "{ x : (hi there) \n hi there}"
