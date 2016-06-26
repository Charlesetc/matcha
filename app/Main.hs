module Main where

import Lib

main :: IO ()
main = putStrLn . show $ Lib.run "{ x : (hi there) ; hi there}"
