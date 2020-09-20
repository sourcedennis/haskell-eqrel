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
-- This module exposes access to the internals of `FrozenEqRel`. This is needed
-- by both `Data.EqRel` and `Data.Frozen.EqRel`, but should not be accessible by
-- library users. Hence, it lives in this module.
--------------------------------------------------------------------------------

module Data.Frozen.EqRelInternal where

-- External library imports
import Data.Map ( Map )
import Data.Set ( Set )


-- | A frozen equivalence relation. While it cannot be modified, it provides
-- fast query times. See `Data.Eqrel` with the `Data.EqRel.freeze` function to
-- construct it.
data FrozenEqRel a =
  FrozenEqRel {
    elements    :: Map a (a, Set a)
  , allClasses  :: [(a, Set a)]
  }
