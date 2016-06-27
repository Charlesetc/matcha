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
ignore_after :: Bool -> Parser a -> (Parser b -> Parser b)
ignore_after beginning p p' = do
  let ignored = optional . many $ p
  if beginning then ignored else return ()
  r <- p'
  ignored
  return r

spaced, lined, twin_lined :: Parser a -> Parser a
spaced = ignore_after False (char ' ')
lined  = ignore_after False newline
twin_lined  = ignore_after True newline

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
definition = lined $ between (schar '{') (schar '}') function where

    function = do
      a <- function_arguments
      b <- function_body
      return $ FDef a b

    line = many1 matcha_token >>= \x -> return (FApp x)

    function_arguments = manyTill symbol (twin_lined $ schar ':')
    function_body = line `sepBy` newline

-- function application
application :: Parser (Tree String)
application = lined $ between (schar '(') (schar ')') (many lined_matcha_token)
    >>= \x -> return (FApp x)

-- any token
matcha_token :: Parser (Tree String)
matcha_token = definition <|> application <|> symbol <|> dot

lined_matcha_token = (do newline; lined_matcha_token) <|> matcha_token

-- parse many tokens and eof
matcha_tokens :: Parser [Tree String]
matcha_tokens = many lined_matcha_token >>= \x -> (eof >> return x)

run = parse matcha_tokens "(unknown)"
