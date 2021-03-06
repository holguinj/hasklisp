module Lisp where

import Parse
import Control.Applicative ((<|>))

data Expression =
    EString String
  | EInteger Integer
  | ESymbol String
  | EList [Expression]
  deriving (Show)

stringExp :: Parser Expression
stringExp = EString <$> quotedString

integerExp :: Parser Expression
integerExp = EInteger <$> integer

symbolExp :: Parser Expression
symbolExp = ESymbol <$> alphanum

listExp :: Parser Expression
listExp = EList <$> (parens contents <|> brackets contents)
  where
    contents :: Parser [Expression]
    contents = expression `sepBy` lispWhitespace <|> return []

expression :: Parser Expression
expression = stringExp
         <|> integerExp
         <|> symbolExp
         <|> listExp

lispWhitespace :: Parser ()
lispWhitespace = many (whitespace <|> comma) >> return ()

example :: Expression
example =
  let testString = unlines [ "(defn foo []"
                            , "  (println \"foo!\")"
                            , "  (sum 1, 2, 3))"
                            ]
      Just (res, _) = parse expression testString
  in
    res
