module Main where

import Tape

import Data.Char
import Data.Maybe
import Data.List

import Control.Monad
import Control.Applicative
import Control.Arrow 

main :: IO ()
main = run ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+."
  where
    run s = interpret $ run' (parse bfParser s)
    run' (Just (bf, _)) = bf


type Brainfuck = [Instruction]

data Instruction = Incr | Decr | Print | Read | SRight | SLeft | Loop Brainfuck | NoOp
  deriving (Show)

interpret :: Brainfuck -> IO ()
interpret bf = void $ interpret' bf zeroes 


interpret' :: Brainfuck -> Tape -> IO Tape
interpret' = foldr ((>=>) . eval) pure


eval :: Instruction -> Tape -> IO Tape
eval Print t = do putChar $ chr (curr t)
                  pure t
eval Read t = undefined
eval (Loop bf) t =
  if curr t == 0 then pure t
  else interpret' bf t >>= eval (Loop bf)
eval NoOp t = pure t
eval ins t = (pure . update) t
  where
  update = case ins of
    Incr -> incr
    Decr -> decr
    SRight -> next
    SLeft -> prev

-- Parsing


bfParser :: Parser Brainfuck
bfParser = many (loopParser <|> instr <|> P (\(x:xs) -> Just (NoOp, xs)))

loopParser :: Parser Instruction 
loopParser = do char '['
                x <- bfParser
                char ']'
                return $ Loop x

instr :: Parser Instruction
instr = fmap toInstr (chars "+-<>,.") 

toInstr :: Char -> Instruction
toInstr c = case c of 
               '+' -> Incr
               '-' -> Decr
               '>' -> SRight
               '<' -> SLeft
               ',' -> Read
               '.' -> Print

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
  fmap g p = P (fmap (first g) . parse p)

instance Applicative Parser where
  pure v = P (\inp -> Just (v, inp))
  pg <*> px = P (\inp -> (parse pg inp) >>= (\(g, out) -> parse (fmap g px) out))

instance Monad Parser where
  p >>= f = P (\inp -> 
    case parse p inp of
      Nothing -> Nothing
      Just (v, out) -> parse (f v) out)

instance Alternative Parser where 
  empty = P (const Nothing)
  p <|> q = P (\inp -> case parse p inp of 
                              Nothing -> parse q inp
			      j -> j)

item :: Parser Char
item = P uncons

sat :: Parser a -> (a -> Bool) -> Parser a
sat p f = do x <- p
             if f x then pure x else empty

char :: Char -> Parser Char
char c = sat item (== c)

chars :: [Char] -> Parser Char
chars cs = sat item (`elem` cs)
