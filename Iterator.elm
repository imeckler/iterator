module Iterator
  ( map
  , upTo
  , range
  , concat
  , concatMap
  , Status(..)
  , foldWhile
  , fold
  , find
  ) where

import Native.Iterator

type Iterator a
  = Fun Int (Int -> a)
  | Cat Int (Int -> Iterator a)

map : (a -> b) -> Iterator a -> Iterator b
map f t = case t of
  Fun n g -> Fun n (f << g)
  Cat n g -> Cat n (map f << g)

-- for : Int -> (Int -> Iterator a) -> Iterator a

upTo : Int -> (Int -> a) -> Iterator a
upTo = Fun

-- inclusive
range : Int -> Int -> Iterator Int
range start stop = upTo (stop - start + 1) (\i -> start + i)

concat : Iterator (Iterator a) -> Iterator a
concat t = case t of
  Fun n g -> Cat n g
  Cat n g -> Cat n (concat << g)

concatMap : (a -> Iterator b) -> Iterator a -> Iterator b
concatMap f = concat << map f

type Status a
  = Finished a
  | KeepGoing a

foldWhile : (a -> b -> Status b) -> Status b -> Iterator a -> b
foldWhile = Native.Iterator.foldWhile

fold : (a -> b -> b) -> b -> Iterator a -> b
fold = Native.Iterator.fold

find : (a -> Bool) -> Iterator a -> Maybe a
find f = foldWhile (\x _ -> if f x then Finished (Just x) else KeepGoing Nothing) (KeepGoing Nothing)

