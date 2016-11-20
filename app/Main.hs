module Main where

import qualified Matcha.Parser as Parser
import qualified Matcha.Dot as Dot
import qualified Matcha.Operator as Operator

maptree = Operator.maptree . Dot.maptree

-- this is an either monad
interaction s = do
  x <- Parser.run s
  return (map maptree x)

main :: IO ()
main = interact (show . interaction)
