--------------------------------------------------------------------------------
-- |
-- Module      : Data.Frozen.EqRel
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- The /frozen/ version of `Data.EqRel`. It provides much faster query times
-- than `Data.EqRel.EqRel`, but cannot be modified.
--
-- This module requires that elements implement the 'Ord' trait. The order of
-- elements does not affect equivalence classes (as these are explicitly
-- defined). When this trait is not available, consider
-- 'Data.Frozen.HashEqRel.FrozenHashEqRel' instead.
--
-- No `FrozenEqRel` can be directly constructed. Instead, construct it by
-- freezing a `Data.EqRel.EqRel` with `Data.EqRel.freeze`.
--------------------------------------------------------------------------------

module Data.Frozen.EqRel
  ( FrozenEqRel
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
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set ( Set )
-- Local imports
import Data.Frozen.EqRelInternal ( FrozenEqRel (..) )


-- Note that `Data.Frozen.EqRelInternal` contains the data structure, as the
-- internals of the structure are used by both this module and `Data.EqRel`.


-- # Query #

-- | /O(log n)/. Returns 'True' iff two elements are equal in the provided
-- equivalence relation.
areEq :: Ord a => a -> a -> FrozenEqRel a -> Bool
areEq a b rel =
  maybe (a == b) (Set.member b . snd) (Map.lookup a (elements rel))

-- | /O(log n)/. Returns all elements in the equivalence class of the provided
-- element.
eqClass :: Ord a => a -> FrozenEqRel a -> Set a
eqClass a rel =
  maybe (Set.singleton a) snd $ Map.lookup a (elements rel)

-- | /O(n)/ in the number of contained equivalence classes. Returns all
-- stored equivalence classes.
--
-- Note that equivalence classes containing only a single element are never
-- returned; As every element is at least equal to itself (by reflexivity), this
-- could produce infinite output when the domain is infinite.
eqClasses :: Ord a => FrozenEqRel a -> [Set a]
eqClasses = map snd . allClasses

-- | /O(log n)/. Returns the representative of the element's equivalence class.
representative :: Ord a => a -> FrozenEqRel a -> a
representative a =
  maybe a fst . Map.lookup a . elements
