{-|
Module      : Data.BinaryIndexedTree.ST
Description : Binary Indexed Trees (a.k.a. Fenwick Trees)
Copyright   : (c) 2012 Maxwell Sayles.
License     : LGPL

Maintainer  : maxwellsayles@gmail.com
Stability   : stable
Portability : portable

Implements mutable binary indexed trees (a.k.a. Fenwick Trees)
in O(logn) for increment and lookup and O(n) for creation.

Original concept from Peter M. Fenwick (1994)
\"/A new data structure for cumulative frequency tables/\"
<http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.8917>.

Index i in the tree represents the sum of all values of
indexes j<=i for some array.

Indexes start at 1.
-}

module Data.BinaryIndexedTree.ST
    (BinaryIndexedTree, new, (!), increment)
where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Bits

{-| Binary Indexed Tree -}
type BinaryIndexedTree s = STUArray s Int Int

{-| Construct a new binary indexed tree on the indexes 1 through n. -}
new :: Int -> ST s (BinaryIndexedTree s)
new n = newListArray (1, n) $ repeat 0

{-| Compute the sum of all indexes 1 through i, inclusive. Takes O(logn). -}
(!) :: BinaryIndexedTree s -> Int -> ST s Int
(!) bit i = f i 0
    where f i acc
              | i < 1 = return acc
              | otherwise =
                  do acc' <- (+acc) <$> readArray bit i
                     let i' = i - (i .&. (-i))
                     f i' acc'

{-| Increment the value at index i. Takes O(logn). -}
increment :: Int -> Int -> BinaryIndexedTree s -> ST s ()
increment i v bit =
    do (_, u) <- getBounds bit
       let f i = when (i <= u) $ do
                   writeArray bit i . (+v) =<< readArray bit i
                   let i' = i + (i .&. (-i))
                   f i'
       f i