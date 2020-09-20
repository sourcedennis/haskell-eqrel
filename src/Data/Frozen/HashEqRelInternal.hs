--------------------------------------------------------------------------------
-- |
-- Module      : Data.Frozen.EqRelInternal
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- See `Data.Frozen.EqRel`.
--
-- This module exposes access to the internals of `FrozenHashEqRel`. This is
-- needed by both `Data.HashEqRel` and `Data.Frozen.HashEqRel`, but should not
-- be accessible by library users. Hence, it lives in this module.
--------------------------------------------------------------------------------

module Data.Frozen.HashEqRelInternal where

-- External library imports
import Data.HashMap.Strict ( HashMap )
import Data.HashSet ( HashSet )


-- | A frozen equivalence relation. While it cannot be modified, it provides
-- fast query times. See `Data.HashEqrel` with the `Data.EqRel.freeze` function
-- to construct it.
data FrozenHashEqRel a =
  FrozenHashEqRel {
    elements    :: HashMap a (a, HashSet a)
  , allClasses  :: [(a, HashSet a)]
  }
