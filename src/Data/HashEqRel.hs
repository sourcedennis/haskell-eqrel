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
-- each function is /amortized/.
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
  , areEq
  , eqClass
  , eqClasses
  , representative
    -- * Combine
  , combine
    -- * Conversion
  , fromList
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
  let ((reprA, aSize), r2) = mapFst (fromMaybe (a, 1)) (representativeWithSize a r)
      ((reprB, bSize), r3) = mapFst (fromMaybe (b, 1)) (representativeWithSize b r2)
  in
  if reprA == reprB then
    r3 -- 'a' and 'b' are already equal
  else if aSize < bSize then
    -- Make the small set point to the big set
    HashEqRel $ HashMap.insert reprA (NodeLink b) $ HashMap.insert reprB (NodeRoot (aSize + bSize)) $ treeMap r3
  else
    -- Make the small set point to the big set
    HashEqRel $ HashMap.insert reprB (NodeLink a) $ HashMap.insert reprA (NodeRoot (aSize + bSize)) $ treeMap r3

-- | /O(m log (n+m))/. Equate all provided elements in the equivalence relation.
-- This unifies the equivalence classes of all provided elements. Note that /m/
-- represents the number of inserted elements, whereas /n/ is the number of
-- elements already in the relation.
equateAll :: (Eq a, Hashable a) => [a] -> HashEqRel a -> HashEqRel a
equateAll xs r = foldr ($) r $ zipWith equate xs (safeTail xs)


-- # Query #

-- | /O(log n)/. Returns 'True' iff two elements are equal in the
-- provided equivalence relation.
--
-- Example:
--
-- > fst $ areEq 1 2 $ equateAll [1,2,7] $ equate [6,8] empty = True
-- > fst $ areEq 1 2 $ equateAll [1,3,7] $ equate [6,8] empty = False
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
areEq :: (Eq a, Hashable a) => a -> a -> HashEqRel a -> (Bool, HashEqRel a)
areEq a b r =
  let (aRepr, r2) = representative a r
      (bRepr, r3) = representative b r2
  in (aRepr == bRepr, r3)

-- | /O(n log n)/. Returns all elements in the equivalence class of
-- the provided element.
--
-- Example:
--
-- > fst $ eqClass 1 $ equateAll [1,4,5] $ equate 7 8 empty =
-- >   fromList [1,4,5]
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
eqClass :: (Eq a, Hashable a) => a -> HashEqRel a -> (HashSet a, HashEqRel a)
eqClass a r@(HashEqRel m) =
  case representativeWithSize a r of
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
    let (bRepr, r2) = representative b r
    in
    if aRepr == bRepr then
      (b:s, r2)
    else
      (s, r2)

-- | /O(n log n)/. Returns all equivalence classes defined in the relation. Note
-- that classes with only a single element are never returned by this function
-- (as those trivially equivalent by reflexivity).
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
eqClasses :: (Eq a, Hashable a) => HashEqRel a -> ([HashSet a], HashEqRel a)
eqClasses r@(HashEqRel m) =
  mapFst HashMap.elems $ foldr step (HashMap.empty, r) $ HashMap.keys m
  where
  step :: (Eq a, Hashable a) => a -> (HashMap a (HashSet a), HashEqRel a) -> (HashMap a (HashSet a), HashEqRel a)
  step a (res, r) =
    let (aRepr, r2) = representative a r
    in
    (HashMap.alter (Just . HashSet.insert a . fromMaybe (HashSet.singleton a)) aRepr res, r2)

-- | /O(n log n)/. Returns the representative of the element's equivalence
-- class.
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
representative :: (Eq a, Hashable a) => a -> HashEqRel a -> (a, HashEqRel a)
representative a = mapFst (maybe a fst) . representativeWithSize a


-- # Combine #

-- | /O(n log n)/. Combines the equivalences contained in both equivalence
-- relations into a new equivalence relation.
--
-- For inputs A and B, and output C, holds the following relation:
-- @(xAy or xBy) => xCy@. Note that C is another equivalence relation, which is
-- reflexive, symmetric, and transitive.
--
-- Example:
--
-- > let a = fromList [[1,6,7],[2,8]]
-- > let b = fromList [[2,3],[8,9]]
-- > combine a b = fromList [[1,6,7],[2,3,8,9]]
combine :: (Eq a, Hashable a) => HashEqRel a -> HashEqRel a -> HashEqRel a
combine a b =
  -- Loop over all equivalence classes in 'b' and insert them into 'a'
  foldr (equateAll . HashSet.toList) a (fst $ eqClasses b)


-- # Conversion #

-- | /O(n log n)/. Constructs a equivalence relation from the given list of
-- equivalence classes.
--
-- Example:
--
-- > let rel = fromList [[1,2],[4,5,6],[7]]
-- > fst (areEquivalent 4 6 rel) = True
-- > fst (areEquivalent 6 7 rel) = False
fromList :: (Eq a, Hashable a) => [[a]] -> HashEqRel a
fromList = foldr equateAll empty


-- # Helpers (Internal) #

-- | Private. /O(log n)/. Returns the representative of the element's
-- equivalence class, together with the number of elements in the equivalence
-- class. If the element is not part of any defined equivalence classes,
-- 'Nothing' is returned as the first element.
--
-- Note that, when 'Nothing' is returned, the element is still in an equivalence
-- class; namely the class containing only the element itself (by reflexivity).
-- However, those classes are not stored in the tree. In that case, use:
-- > mapFst (fromMaybe (a, 1)) (representativeWithSize a r)
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
representativeWithSize :: (Eq a, Hashable a) => a -> HashEqRel a -> (Maybe (a, ClassSize), HashEqRel a)
representativeWithSize a r@(HashEqRel m) =
  case a `HashMap.lookup` m of
    Nothing -> (Nothing, r)
    Just (NodeRoot n) -> (Just (a, n), r)
    Just (NodeLink aNext)   ->
      case representativeWithSize aNext r of
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
  show = show . map HashSet.toList . fst . eqClasses

instance (Eq a, Hashable a) => Semigroup (HashEqRel a) where
  (<>) = combine

instance (Eq a, Hashable a) => Monoid (HashEqRel a) where
  mempty = empty
