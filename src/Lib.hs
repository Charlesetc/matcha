module Lib
    ( run,
      pprint
    ) where

import Text.ParserCombinators.ReadP as R

data Token = Left_c
           | Right_c
           | Left_r
           | Right_r
           deriving Show

-- this is like 'satisfy' but you can control the return value
satisfy_with :: (Char -> Bool) -> (Char -> a) -> ReadP a
satisfy_with p a = do c <- get; if p c then return (a c) else pfail

-- mappings between char and token
token_char char token = satisfy_with (== char) (const token)

parse_tokens = token_char '{' Left_c
             +++ token_char '}' Right_c
             +++ token_char '(' Left_r
             +++ token_char ')' Right_r

-- turn a readP into a function
run :: String -> [(Token, String)]
run = readP_to_S parse_tokens

-- print the output of run
pprint :: [(Token, String)] -> IO ()
pprint = foldr print_tuple (putStrLn "") where
  print_tuple (token, _) acc = acc >> putStrLn (show token)
