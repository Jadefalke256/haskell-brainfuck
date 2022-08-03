module Parser where

import Data.List

import Control.Applicative
import Control.Monad
import Control.Arrow

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
  fmap g p = P (fmap (first g) . parse p)

instance Applicative Parser where
  pure v = P (\inp -> Just (v, inp))
  pg <*> px = P (parse pg >=> (\(g, out) -> parse (fmap g px) out))

instance Monad Parser where
  p >>= f = P (parse p >=> (\(v, out) -> parse (f v) out))

instance Alternative Parser where 
  empty = P (const Nothing)
  p <|> q = P (\inp -> parse p inp <|> parse q inp)


item :: Parser Char
item = P uncons

sat :: Parser a -> (a -> Bool) -> Parser a
sat p f = do x <- p
             if f x then pure x else empty

char :: Char -> Parser Char
char c = sat item (== c)

chars :: [Char] -> Parser Char
chars cs = sat item (`elem` cs)
