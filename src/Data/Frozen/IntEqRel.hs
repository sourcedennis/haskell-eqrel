--------------------------------------------------------------------------------
-- |
-- Module      : Data.Frozen.IntEqRel
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- The /frozen/ version of `Data.IntEqRel`. It provides much faster query times
-- than `Data.IntEqRel.IntEqRel`, but cannot be modified.
--
-- No `FrozenIntEqRel` can be directly constructed. Instead, construct it by
-- freezing a `Data.IntEqRel.IntEqRel` with `Data.IntEqRel.freeze`.
--------------------------------------------------------------------------------

module Data.Frozen.IntEqRel
  ( FrozenIntEqRel
    -- * Query
  , areEq
  , eqClass
  , eqClasses
  , representative
  )
  where

-- Stdlib imports
import           Data.Maybe ( fromMaybe )
-- Extra stdlib imports
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- Local imports
import Data.Frozen.IntEqRelInternal ( FrozenIntEqRel (..) )


-- Note that `Data.Frozen.IntEqRelInternal` contains the data structure, as the
-- internals of the structure are used by both this module and `Data.IntEqRel`.


-- # Query #

-- | /O(log n)/. Returns 'True' iff two elements are equal in the provided
-- equivalence relation.
areEq :: Int -> Int -> FrozenIntEqRel -> Bool
areEq a b rel =
  maybe (a == b) (IntSet.member b . snd) (IntMap.lookup a (elements rel))

-- | /O(log n)/. Returns all elements in the equivalence class of the provided
-- element.
eqClass :: Int -> FrozenIntEqRel -> IntSet
eqClass a rel =
  maybe (IntSet.singleton a) snd $ IntMap.lookup a (elements rel)

-- | /O(n)/ in the number of contained equivalence classes. Returns all
-- stored equivalence classes.
--
-- Note that equivalence classes containing only a single element are never
-- returned; As every element is at least equal to itself (by reflexivity), this
-- could produce infinite output when the domain is infinite.
eqClasses :: FrozenIntEqRel -> [IntSet]
eqClasses = map snd . allClasses

-- | /O(log n)/. Returns the representative of the element's equivalence class.
representative :: Int -> FrozenIntEqRel -> Int
representative a =
  maybe a fst . IntMap.lookup a . elements
