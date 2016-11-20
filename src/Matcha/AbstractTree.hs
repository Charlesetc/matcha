
module Matcha.AbstractTree
  ( AbstractTree(..)
  , Position(..)
  , Tree
  ) where

-- The syntax trees used internally
data AbstractTree a
  = FDef [AbstractTree a] [AbstractTree a]
  | FApp [AbstractTree a]
  | Symbol a
  | DotSymbol a

  | MethodCall a (AbstractTree a) [AbstractTree a]
  deriving (Show, Eq)

type Tree = AbstractTree (String, Position)

data Position = Position { start :: (Int, Int), end :: (Int, Int) }
                deriving (Show, Eq)
