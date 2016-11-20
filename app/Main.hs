module Main where

import Matcha.Parser as Parser
import Matcha.Dot as Dot

-- this is an either monad
interaction s = do
  x <- Parser.run s
  return (map Dot.mapdottree x)

main :: IO ()
main = interact (show . interaction)
