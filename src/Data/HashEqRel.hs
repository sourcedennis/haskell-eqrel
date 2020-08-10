--------------------------------------------------------------------------------
-- |
-- Module      : Data.HashEqRel
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- An equivalence relation where equivalence between elements has to be
-- explicitly stated with the provided functions. The time complexity stated for
-- each function is _amortized_.
--
-- This module requires that elements implement the 'Hashable' trait.
--
-- Note that many function returned a tuple where the second element is another
-- equivalence relation. Often this equivalence relation is (semantically)
-- equivalent to the input relation, but may have an updated internal
-- representation. It is advised to use the returned relation afterwards, as it
-- ensures the amortized time complexity stated for the functions.
--------------------------------------------------------------------------------

module Data.HashEqRel
  ( -- * Types
    HashEqRel
    -- * Construction
  , empty
    -- * Update
  , equate
  , equateAll
    -- * Query
  , areEquivalent
  , equivalenceClass
  , equivalenceClasses
    -- * Combine
  , combine
  ) where

-- Stdlib imports
import           Data.Maybe ( fromMaybe )
-- External library imports
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashSet as HashSet
import           Data.HashSet ( HashSet )
import           Data.Hashable ( Hashable )


-- This module is almost a verbatim copy of `Data.EqRel`. Sadly, very few things
-- can be shared between the two as both require distinct classes (i.e. `Ord` vs
-- `Eq`+`Hashable`)


-- # Types #

-- | Private. The number of elements in an equivalence class.
type ClassSize = Integer

-- | Private. A tree node. Every node represents a single element in the set of
-- equivalence classes.
data Node a
  = NodeRoot ClassSize
    -- ^ The root of the tree containing the class representative. The size of
    -- the equivalence class is stored so class unification is faster. Namely,
    -- when classes are unified, the smaller class comes to refer to the bigger
    -- class.
  | NodeLink a
    -- ^ A child node in the tree. It references another node in the tree. When
    -- following the entire chain of links, eventually a root node is reached.

-- | An equivalence relation. It is reflexive, symmetric, and transitive. Note
-- that equivalent elements have to be explicitly added to this relation.
newtype HashEqRel a = HashEqRel { treeMap :: HashMap a (Node a) }


-- # Construction #

-- | The empty equivalence relation. No distinct elements are equivalent. Every
-- element is only equal to itself (by reflexivity).
empty :: HashEqRel a
empty = HashEqRel HashMap.empty


-- # Update #

-- | /O(log n)/. Equate two elements in the equivalence relation. This unifies
-- the equivalence classes both elements are members of.
equate :: (Eq a, Hashable a) => a -> a -> HashEqRel a -> HashEqRel a
equate a b r =
  let ((reprA, aSize), r2) = mapFst (fromMaybe (a, 1)) (representative a r)
      ((reprB, bSize), r3) = mapFst (fromMaybe (b, 1)) (representative b r2)
  in
  if reprA == reprB then
    r3 -- 'a' and 'b' are already equal
  else if aSize < bSize then
    -- Make the small set point to the big set
    HashEqRel $ HashMap.insert reprA (NodeLink b) $ HashMap.insert reprB (NodeRoot (aSize + bSize)) $ treeMap r3
  else
    -- Make the small set point to the big set
    HashEqRel $ HashMap.insert reprB (NodeLink a) $ HashMap.insert reprA (NodeRoot (aSize + bSize)) $ treeMap r3

-- | /O(m log n)/. Equate all provided elements in the equivalence relation.
-- This unifies the equivalence classes of all elements.
equateAll :: (Eq a, Hashable a) => [a] -> HashEqRel a -> HashEqRel a
equateAll xs r = foldr ($) r $ zipWith equate xs (safeTail xs)


-- # Query #

-- | /O(log n)/. Returns 'True' iff two elements are equal in the
-- provided equivalence relation.
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
areEquivalent :: (Eq a, Hashable a) => a -> a -> HashEqRel a -> (Bool, HashEqRel a)
areEquivalent a b r =
  case representative a r of
    (Nothing,         r2) -> (a == b, r2)
    (Just (aRepr, _), r2) ->
      case representative b r2 of
        (Nothing,         r3) -> (False, r3)
        (Just (bRepr, _), r3) -> (aRepr == bRepr, r3)

-- | /O(n log n)/. Returns all elements in the equivalence class of
-- the provided element.
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
equivalenceClass :: (Eq a, Hashable a) => a -> HashEqRel a -> (HashSet a, HashEqRel a)
equivalenceClass a r@(HashEqRel m) =
  case representative a r of
    -- When 'a' has no representative in the tree, it has simply not been added
    -- to the tree. In that case, 'a' is only equal to itself (by refexivity).
    (Nothing, r2) -> (HashSet.singleton a, r2)
    (Just (aRepr, _), r2) ->
      -- This collapses all paths in the entire tree
      mapFst HashSet.fromList $ foldr (step aRepr) ([a], r2) (HashMap.keys m)
  where
  -- | Adds an element to the list if its representative equals the provided
  -- representative. Otherwise it is not added. In either case the updated
  -- (with collapsed paths) equivalence is returned as second tuple element.
  step :: (Eq a, Hashable a) => a -> a -> ([a], HashEqRel a) -> ([a], HashEqRel a)
  step aRepr b (s, r) =
    case representative b r of
      (Just (bRepr, _), r2) ->
        if aRepr == bRepr then
          (b:s, r2)
        else
          (s, r2)
      -- A key in the tree references nothing. Tree construction ensures this
      -- does not happen.
      (Nothing, _) -> error "Invalid Tree"

-- | /O(n log n)/. Returns all equivalence classes defined in the relation. Note
-- that classes with only a single element are never returned by this function
-- (as those trivially equivalent by reflexivity).
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
equivalenceClasses :: (Eq a, Hashable a) => HashEqRel a -> ([HashSet a], HashEqRel a)
equivalenceClasses r@(HashEqRel m) =
  mapFst HashMap.elems $ foldr step (HashMap.empty, r) $ HashMap.keys m
  where
  step :: (Eq a, Hashable a) => a -> (HashMap a (HashSet a), HashEqRel a) -> (HashMap a (HashSet a), HashEqRel a)
  step a (res, r) =
    case representative a r of
      (Just (aRepr, _), r2) ->
        (HashMap.alter (Just . HashSet.insert a . fromMaybe (HashSet.singleton a)) aRepr res, r2)
      -- A key in the tree references nothing. Tree construction ensures this
      -- does not happen.
      (Nothing, _) -> error "Invalid Tree"


-- # Combine #

-- | /O(n log n)/. Combines the equivalences contained in both equivalence
-- relations into a new equivalence relation.
--
-- For inputs A and B, and output C, holds the following relation:
-- ((a = b) in A) or ((a = b) in B) => ((a = b) in C)
-- Note that C is another equivalence relation, which is reflexive, symmetric,
-- and transitive.
combine :: (Eq a, Hashable a) => HashEqRel a -> HashEqRel a -> HashEqRel a
combine a b =
  -- Loop over all equivalence classes in 'b' and insert them into 'a'
  foldr (equateAll . HashSet.toList) a (fst $ equivalenceClasses b)


-- # Helpers (Internal) #

-- | Private. /O(log n)/. Returns the representative of the equivalence class
-- that the provided element is a member of. If the element is not part of any
-- defined equivalence classes, 'Nothing' is returned as the first element.
--
-- Note that, when 'Nothing' is returned, the element is still in an equivalence
-- class; namely the class containing only the element itself (by reflexivity).
-- However, those classes are not stored in the tree. In that case, use:
-- > mapFst (fromMaybe (a, 1)) (representative a r)
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
representative :: (Eq a, Hashable a) => a -> HashEqRel a -> (Maybe (a, ClassSize), HashEqRel a)
representative a r@(HashEqRel m) =
  case a `HashMap.lookup` m of
    Nothing -> (Nothing, r)
    Just (NodeRoot n) -> (Just (a, n), r)
    Just (NodeLink aNext)   ->
      case representative aNext r of
        -- Collapse the path
        (Just (repr, n), r2) -> (Just (repr, n), HashEqRel $ HashMap.insert a (NodeLink repr) $ treeMap r2)
        -- A link node points nowhere. Tree construction ensures this does not
        -- happen.
        (Nothing, _)  -> error "Invalid Tree"

-- | Applies the function to the first element of the tuple.
mapFst :: ( a -> c ) -> ( a, b ) -> ( c, b )
mapFst f (a, b) = (f a, b)

-- | Prelude's 'tail' function throws an error when given the empty list. This
-- implementation simply returns the empty list as the tail of the empty list.
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs


-- # Class instances

instance (Eq a, Hashable a, Show a) => Show (HashEqRel a) where
  show = show . map HashSet.toList . fst . equivalenceClasses

instance (Eq a, Hashable a) => Semigroup (HashEqRel a) where
  (<>) = combine

instance (Eq a, Hashable a) => Monoid (HashEqRel a) where
  mempty = empty
