module Tape where


data Tape = Tape [Int] Int [Int]

zeroes :: Tape
zeroes = Tape (repeat 0) 0 (repeat 0)

curr :: Tape -> Int
curr (Tape _ n _) = n

next :: Tape -> Tape
next (Tape ls n (r:rs)) = Tape (n:ls) r rs

prev :: Tape -> Tape
prev (Tape (l:ls) n rs) = Tape ls l (n:rs)

apply :: (Int -> Int) -> Tape -> Tape
apply f (Tape ls n rs) = Tape ls (f n) rs

incr :: Tape -> Tape
incr = apply (+1)

decr :: Tape -> Tape
decr = apply (\x -> x - 1)
