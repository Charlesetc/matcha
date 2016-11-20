module Matcha.Parser
    ( run
    ) where

import Text.ParserCombinators.Parsec hiding (newline)

import Matcha.AbstractTree

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

into constructor x = return $ constructor x

-- alphanumber :: Parser String
alphanumber = many1 . noneOf $ ";.:(){} \t\n"

symbol :: Parser Tree
symbol = parse_str_symbol >>= into Symbol where
    parse_str_symbol = positioned . spaced $ alphanumber

newline :: Parser Char
newline = spaced $ oneOf ";\n"

dot_symbol :: Parser Tree
dot_symbol = try parse_dot_symbol >>= into DotSymbol where
  parse_dot_symbol = positioned . spaced $ string "." >> alphanumber

dot :: Parser Tree
dot = (positioned . spaced $ string ".") >>= into Symbol

schar :: Char -> Parser Char
schar = spaced . char

-- function definition
definition :: Parser Tree
definition = lined $ between (schar '{') (schar '}') function where

    function = do
      a <- function_arguments
      b <- function_body
      return $ FDef a b

    line = many1 matcha_token >>= into FApp

    function_arguments = manyTill symbol (twin_lined $ schar ':')
    function_body = line `sepBy` newline

-- function application
application :: Parser Tree
application = lined $ between (schar '(') (schar ')') (many lined_matcha_token)
    >>= \x -> return (FApp x)

-- any token
matcha_token :: Parser Tree
matcha_token = definition <|> application <|> symbol <|> dot_symbol <|> dot

lined_matcha_token = (do newline; lined_matcha_token) <|> matcha_token

-- parse many tokens and eof
matcha_tokens :: Parser [Tree]
matcha_tokens = many lined_matcha_token >>= \x -> (eof >> return x)

run = parse matcha_tokens "(unknown)"

-- get the position of a parser and include it in the return.
positioned :: Parser a -> Parser (a, Position)
positioned p = do
  start <- getPosition
  out <- p
  end <- getPosition

  let position = Position s e where
      s = (sourceLine start, sourceColumn start)
      e = (sourceLine end, sourceColumn end)
  return (out, position)
