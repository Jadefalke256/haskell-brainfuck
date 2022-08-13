{-# LANGUAGE LambdaCase#-}

module Main where

import Tape
import Parser

import Data.Char
import Data.Maybe
import Data.List
import Data.Word

import Control.Monad
import Control.Applicative
import Control.Arrow 

import System.Environment
import System.IO
import Data.Maybe (catMaybes)

main :: IO ()
main = do args <- getArgs
          handle args
  where
    handle :: [String] -> IO ()
    handle ["file", fName] = readFile fName >>= interpret
    handle ["string", str] = interpret str >> (print $ parse bfParser str)
    handle _ = putStrLn "could not find command"

type Brainfuck = [Instruction]

data Instruction = Incr | Decr | Print | Read | SRight | SLeft | Loop Brainfuck
  deriving (Show)

interpret :: String -> IO ()
interpret bfc = maybe (putStrLn "oop") (\r -> void $ interpret' (fst r) zeroes) (parse bfParser bfc)

interpret' :: Brainfuck -> Tape -> IO Tape
interpret' = foldr ((>=>) . eval) pure


eval :: Instruction -> Tape -> IO Tape
eval ins t =
    case ins of 
      Print -> putChar (chr $ curr t) >> pure t 
      Read -> getByte >>= (\b -> pure $ apply (const b) t)
      (Loop bf) 
        | curr t == 0 -> pure t
        | otherwise -> interpret' bf t >>= eval ins
      Incr -> pure (incr t)
      Decr -> pure (decr t)
      SRight -> pure (next t)
      SLeft -> pure (prev t)

getByte :: IO Int
getByte = do
  hSetBinaryMode stdin True
  c <- getChar
  hSetBinaryMode stdin False
  pure $! digitToInt c


-- Parsing
bfParser :: Parser Brainfuck
bfParser = catMaybes <$> many (j loopParser <|> j instr <|> noop)
  where j = fmap Just

noop :: Parser (Maybe a)
noop = Nothing <$ sat item (`notElem` "[]+-,.<>")

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
