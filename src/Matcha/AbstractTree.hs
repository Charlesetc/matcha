
module Matcha.AbstractTree
  ( AbstractTree(..)
  , Position(..)
  , Tree
  , maptree'
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

maptree' :: (Tree -> Tree) -> Tree -> Tree
maptree' f t = f (new_tree t) where
  mmaptree = map (maptree' f)
  new_tree (FDef args body) = FDef (mmaptree args) (mmaptree body)
  new_tree (FApp trees) = FApp $ mmaptree trees
  new_tree (otherwise) = otherwise
