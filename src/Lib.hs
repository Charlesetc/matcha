module Lib
    ( run,
    ) where

import Text.ParserCombinators.Parsec hiding (newline)

data Tree a = FDef [Tree a] [Tree a]
          | FApp [Tree a]
          | Symbol a
          deriving (Show)

ignore_after :: Parser a -> (Parser b -> Parser b)
ignore_after p p' = do
  r <- p'
  optional (many $ p)
  return r

spaced :: Parser a -> Parser a
spaced = ignore_after (char ' ')

symbol :: Parser (Tree String)
symbol = spaced (many1 (noneOf ";.:(){} \t\n")) >>= \x -> return (Symbol x)

newline = spaced $ oneOf ";\n"

schar = spaced . char

-- function definition
definition = between (schar '{') (schar '}') function where

    function = do
      a <- function_arguments
      b <- function_body
      return $ FDef a b

    line = ignore_after newline (many1 matcha_token >>= \x -> return (FApp x))

    function_arguments = manyTill symbol (schar ':')
    function_body = many line

-- function application
application = between (schar '(') (schar ')') (many matcha_token)
    >>= \x -> return (FApp x)

-- any token
matcha_token = definition <|> application <|> symbol

-- parse many tokens and eof
matcha_tokens = many matcha_token >>= \x -> (eof >> return x)

run = parse matcha_tokens "(unknown)"
