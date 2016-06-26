module Main where

import Lib

main :: IO ()
main = Lib.pprint $ Lib.run "(hi there)"
