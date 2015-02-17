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
  , Iterator
  ) where

{-| An `Iterator a` can be thought of as a function `Int -> a` along with
an `Int` indicating its "length". It's a simple data type supporting mapping,
concatenation, and most importantly, folding in constant space with early return.

# Introduction
@docs upTil, range

# Transformation
@docs map, concat, concatMap

# Elimination
@docs fold, Status, foldWhile, find
-}

import Native.Iterator

type Iterator a
  = Fun Int (Int -> a)
  | Cat Int (Int -> Iterator a)

{-| Map over the values of the iterator. -}
map : (a -> b) -> Iterator a -> Iterator b
map f t = case t of
  Fun n g -> Fun n (f << g)
  Cat n g -> Cat n (map f << g)

{-| `upTil n f` is conceptually the sequence 
    [f 0, f 1,..., f (n - 1)]
-}
upTil : Int -> (Int -> a) -> Iterator a
upTil = Fun

{-| `range start stop` is conceptually the sequence
    [start, start + 1,...,stop]
-}
range : Int -> Int -> Iterator Int
range start stop = upTil (stop - start + 1) (\i -> start + i)

{-| Concatenate an iterator of iterators. -}
concat : Iterator (Iterator a) -> Iterator a
concat t = case t of
  Fun n g -> Cat n g
  Cat n g -> Cat n (concat << g)

{-|
    concatMap f = concat << map f
-}
concatMap : (a -> Iterator b) -> Iterator a -> Iterator b
concatMap f = concat << map f

{-| This type helps us control early return from a fold. -}
type Status a
  = Finished a
  | KeepGoing a

{-| `foldWhile` allows you to fold, returning early if you like, in constant space (stack or otherwise).
Suppose for example that we'd like to find the first element in a sequence of `a`'s satisfying some property
`p : a -> Bool`. We can express this as a fold over our sequence (assuming it supports some notion of `fold`) as

    fold (\x r -> if p x then Just x else r) Nothing

but we would like for this fold to return early with the first `x` satisfying `p`.
So, we have foldWhile which stops folding once a `Finished x` value is found. We
can thus express `find : (a -> Bool) -> Iterator a -> Maybe a` as

  find f = foldWhile (\x _ -> if f x then Finished (Just x) else KeepGoing Nothing)
             (KeepGoing Nothing)
-}
foldWhile : (a -> b -> Status b) -> Status b -> Iterator a -> b
foldWhile = Native.Iterator.foldWhile

{-| Folds until the bitter end. -}
fold : (a -> b -> b) -> b -> Iterator a -> b
fold = Native.Iterator.fold

{-| `find` the first element in your sequence satisfying the given property. -}
find : (a -> Bool) -> Iterator a -> Maybe a
find f = foldWhile (\x _ -> if f x then Finished (Just x) else KeepGoing Nothing) (KeepGoing Nothing)

