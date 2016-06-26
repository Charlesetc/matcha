module Matcha.Parser
    ( run,
    ) where

import Text.ParserCombinators.Parsec hiding (newline)


data Tree a
  = FDef [Tree a] [Tree a] -- { [identifier] : [FApp] }
  | FApp [Tree a]          -- ([matcha_token])
  | Symbol a
  deriving (Show, Eq)

-- ignores the first parser while keeping the second one.
ignore_after :: Parser a -> (Parser b -> Parser b)
ignore_after p p' = do
  r <- p'
  optional (many $ p)
  return r

spaced :: Parser a -> Parser a
spaced = ignore_after (char ' ')

symbol :: Parser (Tree String)
symbol = spaced (many1 (noneOf ";.:(){} \t\n")) >>= \x -> return (Symbol x)

newline :: Parser Char
newline = spaced $ oneOf ";\n"

dot :: Parser (Tree String)
dot = char '.' >>= \x -> return (Symbol ".")

schar :: Char -> Parser Char
schar = spaced . char

-- function definition
definition :: Parser (Tree String)
definition = between (schar '{') (schar '}') function where

    function = do
      a <- function_arguments
      b <- function_body
      return $ FDef a b

    line = ignore_after newline (many1 matcha_token >>= \x -> return (FApp x))

    function_arguments = manyTill symbol (schar ':')
    function_body = many line

-- function application
application :: Parser (Tree String)
application = between (schar '(') (schar ')') (many matcha_token)
    >>= \x -> return (FApp x)

-- any token
matcha_token :: Parser (Tree String)
matcha_token = definition <|> application <|> symbol <|> dot

-- parse many tokens and eof
matcha_tokens :: Parser [Tree String]
matcha_tokens = many matcha_token >>= \x -> (eof >> return x)

run = parse matcha_tokens "(unknown)"
