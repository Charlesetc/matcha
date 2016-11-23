module Matcha.Operator
    ( maptree
    , operators
    , precedence
    , shunting_yard
    ) where

import Matcha.AbstractTree
import Data.Maybe (fromJust)

{--

  This file's purpose is to do the shunting yard algorithm
  and convert operators to function calls with appropriate
  precedence. The operators are hard-coded.

 --}

data Associativity = ALeft | ARight
  deriving (Show, Eq)

-- the global data defining the operators here
operators :: [[(String, Associativity)]]
operators = [ [("/", ALeft), ("*", ALeft)]
            , [("-", ALeft), ("+", ALeft)]
            ]

operatorSymbols :: [String]
operatorSymbols = concatMap (map (fst)) operators

precedence :: String -> Int
precedence symbol = inner_precedence operators where
  inner_precedence [] = error "Symbol not found"
  inner_precedence (hd : rest) =
    if elem symbol (map fst hd)
       then 0
       -- because I like listing the higher precedence first:
       else 1 - inner_precedence rest

associativity :: String -> Associativity
associativity symbol = inner_assoc (concat operators) where
  inner_assoc [] = error "Symbol not fund"
  inner_assoc (hd : rest)
    | fst hd == symbol = snd hd
    | otherwise = inner_assoc rest

shunting_yard :: Tree -> Tree
shunting_yard (FApp trees) = FApp . convert_from_postfix $ syard [] [] trees where

  convert_from_postfix :: [Tree] -> [Tree]
  convert_from_postfix [] = []
  convert_from_postfix (t : trees) =
    let new_trees = convert_from_postfix trees in
    case new_trees of
      (a1 : a2 : even_newer_trees) | tree_is_operator t -> FApp [t, a1, a2] : even_newer_trees
      _ | tree_is_operator t -> error "Binary operator with only one argument"
      _ -> (t : new_trees)

  -- feel free to pretend syard stands for 'scotland yard'
  syard :: [Tree] -> [Tree] -> [Tree] -> [Tree]
  syard operators output [] = operators ++ output
  syard [] output (t : trees) = if tree_is_operator t
                           then syard [t] output trees
                           else syard [] (t : output) trees

  syard (o : operators) output (t : trees) | tree_is_operator t =
    -- since the tree is an operator, it's also a symbol
    let symbol = unsafe_symbol_of_tree t in
    case associativity symbol of
      ALeft -> if precedence symbol <= precedence (unsafe_symbol_of_tree o)
                  then syard operators (o : output) (t : trees)
                  else syard (t : o : operators) output trees

      ARight -> if precedence symbol > precedence (unsafe_symbol_of_tree o)
                   -- do the same thing as earlier
                   then syard operators (o : output) (t : trees)
                   else syard (t : o : operators) output trees

  -- where it's not an operator
  syard operators output (t : trees) = syard operators (t : output) trees


  -- is this tree this Symbol?
  symbol_of_tree (Symbol (s, _)) = Just s
  symbol_of_tree _ = Nothing

  unsafe_symbol_of_tree = fromJust . symbol_of_tree

  tree_is_operator :: Tree -> Bool
  tree_is_operator t =
    case symbol_of_tree t of
      Just t -> t `elem` operatorSymbols
      Nothing -> False

shunting_yard other = other

maptree = maptree' shunting_yard
