{-
Implements persistent binary indexed trees (or Fenwick Trees)
in O(logn) for increment and lookup and O(n) for creation.

Index i in the tree represents the sum of all values of
indexes j<=i for some array.

The idea is that for k bits, we parse the index i from msb
to lsb and move left/right on the tree for 0/1.

For a read, we accumulate the values in the tree where the
binary representation of the index contains a 1. (The
technique is similar to binary exponentiation.)

For an increment (update), we should increment parent nodes
in the tree whose corresponding binary index representation
is >= than the index i.
-}

module Data.BinaryIndexedTree
    (BinaryIndexedTree, new, (!), increment)
where

import Data.Bits

{-| A Binary indexed tree. -}
data BinaryIndexedTree a = BinaryIndexedTree Int (Tree a)

data Tree a = Empty | Node a (Tree a) (Tree a)

{-| 
Construct a binary indexed tree on k bits. Takes O(n).
-}
new :: Num a => Int -> BinaryIndexedTree a
new k = BinaryIndexedTree k $ f k
    where f 0 = Empty
          f k = Node 0 (f (k - 1)) (f (k - 1))

{-|
Lookup the sum of all values from index 1 to index i. Takes O(logn).
-}
(!) :: Num a => BinaryIndexedTree a -> Int -> a
(!) (BinaryIndexedTree k root) i = f root (k - 1) 0
    where f Empty _ acc     = acc
          f (Node x l r) j acc
            | i `testBit` j = acc' `seq` f l j' acc'
            | otherwise     = f r j' acc
            where j'   = j - 1
                  acc' = acc + x

{-|
Increment the value at index i by amount x. Takes O(logn).
-}
increment :: Num a => Int -> a -> BinaryIndexedTree a -> BinaryIndexedTree a
increment i x (BinaryIndexedTree k root) =
    BinaryIndexedTree k $ f root (k - 1) 0
    where f (Node y l r) j acc
            | i `testBit` j =
                if acc' == i
                    then  y'   `seq` Node y' l r
                    else  acc' `seq` Node y (f l j' acc') r
            | otherwise = y'   `seq` Node y' l (f r j' acc)
            where y'   = x + y
                  j'   = j - 1
                  acc' = acc `setBit` j

                         