--------------------------------------------------------------------------------
-- |
-- Module      : Data.Frozen.IntEqRelInternal
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- See `Data.Frozen.IntEqRel`.
--
-- This module exposes access to the internals of `FrozenIntEqRel`. This is
-- needed by both `Data.IntEqRel` and `Data.Frozen.IntEqRel`, but should not be
-- accessible by library users. Hence, it lives in this module.
--------------------------------------------------------------------------------

module Data.Frozen.IntEqRelInternal where

-- External library imports
import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )


-- | A frozen equivalence relation. While it cannot be modified, it provides
-- fast query times. See `Data.Eqrel` with the `Data.EqRel.freeze` function to
-- construct it.
data FrozenIntEqRel =
  FrozenIntEqRel {
    elements    :: IntMap (Int, IntSet)
  , allClasses  :: [(Int, IntSet)]
  }
