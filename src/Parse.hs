module Parse where

import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<|>))
import qualified Data.Char as Char

type ParseResult = Maybe

type Parser a = StateT String ParseResult a

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT

satisfies :: (Char -> Bool) -> Parser String
satisfies f = do
  state <- get
  let (result, newState) = span f state
  if null result
    then lift Nothing
    else do put newState
            return result

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser sep = do
  first <- parser
  rest <- reverse <$> parseRest [] parser sep
  return $ (first:rest)
  where
    parseRest :: [a] -> Parser a -> Parser b -> Parser [a]
    parseRest acc parser sep =
      (do sep
          next <- parser
          parseRest (next:acc) parser sep)
      <|> return acc

optional :: Parser a -> Parser ()
optional parser = (parser >> return ()) <|> return ()

char :: Char -> Parser Char
char c = do
  state <- get
  (x, newState) <- lift $ destruct state
  if x == c
    then do put newState
            return x
    else lift Nothing

whitespace :: Parser ()
whitespace = satisfies (flip elem ['\t', '\n', ' ']) >> return ()

comma :: Parser ()
comma = char ',' >> return ()

integer :: Parser Integer
integer = do
  numString <- satisfies Char.isDigit
  return $ read numString

between :: (Char, Char) -> Parser a -> Parser a
between (start, end) parser = do
  char start
  result <- parser
  char end
  return $ result

quoted :: Parser a -> Parser a
quoted = between ('"', '"')

quotedString :: Parser String
quotedString = quoted $ satisfies (/= '"')

parens :: Parser a -> Parser a
parens = between ('(', ')')

tuple :: Parser a -> Parser b -> Parser (a, b)
tuple aParse bParse = parens $ do
  a <- aParse
  comma
  optional whitespace
  b <- bParse
  return (a, b)

integerList :: Parser [Integer]
integerList = integer `sepBy` (comma >> optional whitespace)

-- helpers

destruct :: [a] -> Maybe (a, [a])
destruct [] = Nothing
destruct (x:xs) = Just (x, xs)
