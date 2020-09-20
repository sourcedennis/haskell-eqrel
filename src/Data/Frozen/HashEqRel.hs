--------------------------------------------------------------------------------
-- |
-- Module      : Data.Frozen.HashEqRel
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- The /frozen/ version of `Data.HashEqRel`. It provides much faster query times
-- than `Data.HashEqRel.HashEqRel`, but cannot be modified.
--
-- This module requires that elements implement the 'Hashable' trait.
--
-- No `FrozenHashEqRel` can be directly constructed. Instead, construct it by
-- freezing a `Data.HashEqRel.HashEqRel` with `Data.HashEqRel.freeze`.
--------------------------------------------------------------------------------

module Data.Frozen.HashEqRel
  ( FrozenHashEqRel
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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import           Data.HashSet ( HashSet )
import           Data.Hashable ( Hashable )
-- Local imports
import Data.Frozen.HashEqRelInternal ( FrozenHashEqRel (..) )


-- Note that `Data.Frozen.HashEqRelInternal` contains the data structure, as the
-- internals of the structure are used by both this module and `Data.HashEqRel`.


-- # Query #

-- | /O(log n)/. Returns 'True' iff two elements are equal in the provided
-- equivalence relation.
areEq :: (Eq a, Hashable a) => a -> a -> FrozenHashEqRel a -> Bool
areEq a b rel =
  maybe (a == b) (HashSet.member b . snd) (HashMap.lookup a (elements rel))

-- | /O(log n)/. Returns all elements in the equivalence class of the provided
-- element.
eqClass :: (Eq a, Hashable a) => a -> FrozenHashEqRel a -> HashSet a
eqClass a rel =
  maybe (HashSet.singleton a) snd $ HashMap.lookup a (elements rel)

-- | /O(n)/ in the number of contained equivalence classes. Returns all
-- stored equivalence classes.
--
-- Note that equivalence classes containing only a single element are never
-- returned; As every element is at least equal to itself (by reflexivity), this
-- could produce infinite output when the domain is infinite.
eqClasses :: (Eq a, Hashable a) => FrozenHashEqRel a -> [HashSet a]
eqClasses = map snd . allClasses

-- | /O(log n)/. Returns the representative of the element's equivalence class.
representative :: (Eq a, Hashable a) => a -> FrozenHashEqRel a -> a
representative a =
  maybe a fst . HashMap.lookup a . elements
