module Eval where

import Parser
import Text.Printf (printf)
import Control.Applicative

-- expr:: = expr + term | expr - term | term
-- term:: = term * factor | term / factor | factor
-- factor:: = (expr) | float
-- float:: = ...

-- expr :: = term expr'
-- expr':: = + term expr' | - term expr' | e
-- term :: = factor term'
-- term' :: = * factor term' | / factor term' | e
-- factor :: = (expr) | float
-- float :: = ...

expr :: Parser Float
expr = do
  t <- term
  expr' t

expr' :: Float -> Parser Float
expr' acc = (do
  symbol "+"
  t <- term
  expr' (acc + t)) <|> (do
  symbol "-"
  t <- term
  expr' (acc - t)) <|> pure acc

term :: Parser Float
term = do
  f <- factor
  term' f

term' :: Float -> Parser Float
term' acc = (do
  symbol "*"
  f <- factor
  term' (acc * f)) <|> (do
  symbol "/"
  f <- factor
  term' (acc / f)) <|> pure acc

factor :: Parser Float
factor = (do
  symbol "("
  e <- expr
  symbol ")"
  return e) <|> float

eval:: String-> Float
eval xs = case (run expr xs) of
        [(n,[])]-> read (printf "%.4f" n)
        [(_,out)]-> error ("Unused Input "++ out)
        []->error "Invalid Input"
        _->error "Invalid Input"
