--------------------------------------------------------------------------------
-- |
-- Module      : Data.EqRel
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
-- This module requires that elements implement the 'Ord' trait. The order of
-- elements does not affect equivalence classes (as these are explicitly
-- defined). When this trait is not available, consider `Data.HashEqRel`
-- instead.
--
-- Note that many function returned a tuple where the second element is another
-- equivalence relation. Often this equivalence relation is (semantically)
-- equivalent to the input relation, but may have an updated internal
-- representation. It is advised to use the returned relation afterwards, as it
-- ensures the amortized time complexity stated for the functions.
--------------------------------------------------------------------------------

module Data.EqRel
  ( -- * Types
    EqRel
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
import qualified Data.Map as Map
import           Data.Map ( Map )
import qualified Data.Set as Set
import           Data.Set ( Set )


-- This module is almost a verbatim copy of `Data.HashEqRel`. Sadly, very few
-- things can be shared between the two as both require distinct classes (i.e.,
-- `Ord` vs `Eq`+`Hashable`)


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
newtype EqRel a = EqRel { treeMap :: Map a (Node a) }


-- # Construction #

-- | The empty equivalence relation. No distinct elements are equivalent. Every
-- element is only equal to itself (by reflexivity).
empty :: EqRel a
empty = EqRel Map.empty

-- | /O(log n)/. Equate two elements in the equivalence relation. This unifies
-- the equivalence classes both elements are members of.
equate :: Ord a => a -> a -> EqRel a -> EqRel a
equate a b r =
  let ((reprA, aSize), r2) = mapFst (fromMaybe (a, 1)) (representative a r)
      ((reprB, bSize), r3) = mapFst (fromMaybe (b, 1)) (representative b r2)
  in
  if reprA == reprB then
    r3 -- 'a' and 'b' are already equal
  else if aSize < bSize then
    -- Make the small set point to the big set
    EqRel $ Map.insert reprA (NodeLink b) $ Map.insert reprB (NodeRoot (aSize + bSize)) $ treeMap r3
  else
    -- Make the small set point to the big set
    EqRel $ Map.insert reprB (NodeLink a) $ Map.insert reprA (NodeRoot (aSize + bSize)) $ treeMap r3

-- | /O(m log n)/. Equate all provided elements in the equivalence relation.
-- This unifies the equivalence classes of all elements.
equateAll :: Ord a => [a] -> EqRel a -> EqRel a
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
areEquivalent :: Ord a => a -> a -> EqRel a -> (Bool, EqRel a)
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
equivalenceClass :: Ord a => a -> EqRel a -> (Set a, EqRel a)
equivalenceClass a r@(EqRel m) =
  case representative a r of
    -- When 'a' has no representative in the tree, it has simply not been added
    -- to the tree. In that case, 'a' is only equal to itself (by refexivity).
    (Nothing, r2) -> (Set.singleton a, r2)
    (Just (aRepr, _), r2) ->
      -- This collapses all paths in the entire tree
      mapFst Set.fromAscList $ foldr (step aRepr) ([a], r2) (Map.keys m)
  where
  -- | Adds an element to the list if its representative equals the provided
  -- representative. Otherwise it is not added. In either case the updated
  -- (with collapsed paths) equivalence is returned as second tuple element.
  step :: Ord a => a -> a -> ([a], EqRel a) -> ([a], EqRel a)
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
equivalenceClasses :: Ord a => EqRel a -> ([Set a], EqRel a)
equivalenceClasses r@(EqRel m) =
  mapFst Map.elems $ foldr step (Map.empty, r) $ Map.keys m
  where
  step :: Ord a => a -> (Map a (Set a), EqRel a) -> (Map a (Set a), EqRel a)
  step a (res, r) =
    case representative a r of
      (Just (aRepr, _), r2) ->
        (Map.alter (Just . Set.insert a . fromMaybe (Set.singleton a)) aRepr res, r2)
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
combine :: Ord a => EqRel a -> EqRel a -> EqRel a
combine a b =
  -- Loop over all equivalence classes in 'b' and insert them into 'a'
  foldr (equateAll . Set.toList) a (fst $ equivalenceClasses b)

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
representative :: Ord a => a -> EqRel a -> (Maybe (a, ClassSize), EqRel a)
representative a r@(EqRel m) =
  case a `Map.lookup` m of
    Nothing -> (Nothing, r)
    Just (NodeRoot n) -> (Just (a, n), r)
    Just (NodeLink aNext)   ->
      case representative aNext r of
        -- Collapse the path
        (Just (repr, n), r2) -> (Just (repr, n), EqRel $ Map.insert a (NodeLink repr) $ treeMap r2)
        -- A link node points nowhere. Tree construction ensures this does not
        -- happen.
        (Nothing, _)  -> error "Invalid Tree"


-- # Helpers (Internal) #

-- | Applies the function to the first element of the tuple.
mapFst :: ( a -> c ) -> ( a, b ) -> ( c, b )
mapFst f (a, b) = (f a, b)

-- | Prelude's 'tail' function throws an error when given the empty list. This
-- implementation simply returns the empty list as the tail of the empty list.
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs


-- # Class instances

instance (Ord a, Show a) => Show (EqRel a) where
  show = show . map Set.toList . fst . equivalenceClasses

instance Ord a => Semigroup (EqRel a) where
  (<>) = combine

instance Ord a => Monoid (EqRel a) where
  mempty = empty
