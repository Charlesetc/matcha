
module Matcha.AbstractTree
  ( AbstractTree(..)
  , Position(..)
  , Tree
  , maptree
  ) where

maptree :: (a -> b) -> AbstractTree a -> AbstractTree b
maptree f tree = case tree of
  FDef a b -> FDef (map (maptree f) a) (map (maptree f) b)
  FApp a -> FApp (map (maptree f) a)
  Symbol a -> Symbol (f a)

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