
module Matcha.AbstractTree
  ( AbstractTree(..)
  , Position(..)
  , Tree
  , maptree'
  ) where

{--

  This tree represents the source of the program and allows
  for easy manipulations on it once it has been parsed.

  There's also a function maptree' which can be used to map
  a function of type Tree->Tree over a tree. Very much like
  an ast_mapper in ocaml.

 --}

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
  lifted_maptree = map (maptree' f)
  new_tree (FDef args body) = FDef (lifted_maptree args) (lifted_maptree body)
  new_tree (FApp trees) = FApp $ lifted_maptree trees
  new_tree (otherwise) = otherwise
