
module Matcha.Dot
  ( maptree
  ) where

import Matcha.AbstractTree

{--

   The point of this file/function is to move .style method
   calls to their own part of the ast.

   For example, "this.that" is parsed as
   FApp([Symbol("this"), Symbol("that")]) which is transformed into
   MethodCall("that", Symbol("this"), []).

   The second argument is the receiver and the third
   argument is the list if arguments to the method
   call.

 --}

maptree :: Tree -> Tree
maptree (FApp applist) =  convertapplist applist where

  convertapplist :: [Tree] -> Tree
  convertapplist (first : ((DotSymbol symb) : rest)) =
    MethodCall symb first rest
  convertapplist other = FApp (convertinnerlist other)

  convertinnerlist [] = []
  convertinnerlist (first : ((DotSymbol symb) : rest)) =
    MethodCall symb first [] : convertinnerlist rest
  convertinnerlist (first : rest) = first : convertinnerlist rest

mapdottree other = other
