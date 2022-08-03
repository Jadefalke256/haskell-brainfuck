module Main where

import Tape
import Parser

import Data.Char
import Data.Maybe
import Data.List

import Control.Monad
import Control.Applicative
import Control.Arrow 

import System.Environment


main :: IO ()
main = do args <- getArgs
          handle args
  where
    handle :: [String] -> IO ()
    handle ["file", fName] = readFile fName >>= interpret
    handle ["string", str] = interpret str
    handle _ = putStrLn "could not find command"

type Brainfuck = [Instruction]

data Instruction = Incr | Decr | Print | Read | SRight | SLeft | Loop Brainfuck
  deriving (Show)

interpret :: String -> IO ()
interpret bfc = maybe (putStrLn "oop") (\r -> void $ interpret' (fst r) zeroes) (parse bfParser bfc)

interpret' :: Brainfuck -> Tape -> IO Tape
interpret' = foldr ((>=>) . eval) pure


eval :: Instruction -> Tape -> IO Tape
eval Print t = putChar (chr (curr t)) >> pure t
eval Read t = undefined
eval (Loop bf) t 
  | curr t == 0 = pure t
  | otherwise = interpret' bf t >>= eval (Loop bf)
eval ins t = (pure . update) t
  where
  update = case ins of
    Incr -> incr
    Decr -> decr
    SRight -> next
    SLeft -> prev
    _ -> error "unreachable"

-- Parsing
bfParser :: Parser Brainfuck
bfParser = many (loopParser <|> instr)

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
               _ -> error "unreachable"
