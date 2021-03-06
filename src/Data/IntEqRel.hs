--------------------------------------------------------------------------------
-- |
-- Module      : Data.IntEqRel
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
-- explicitly stated with the provided functions. Elements are represented by
-- the system ints. The time complexity stated for each function is /amortized/.
--
-- Note that many function returned a tuple where the second element is another
-- equivalence relation. Often this equivalence relation is (semantically)
-- equivalent to the input relation, but may have an updated internal
-- representation. It is advised to use the returned relation afterwards, as it
-- ensures the amortized time complexity stated for the functions.
--------------------------------------------------------------------------------

module Data.IntEqRel
  ( -- * Types
    IntEqRel
    -- * Construction
  , empty
    -- * Update
  , equate
  , equateAll
  , collapse
    -- * Query
  , areEq
  , eqClass
  , eqClasses
  , representative
    -- * Combine
  , combine
    -- * Conversion
  , fromList
  , freeze
  ) where

-- Stdlib imports
import           Data.Maybe ( fromMaybe )
-- Extra stdlib imports
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- Local imports
import           Data.Frozen.IntEqRelInternal ( FrozenIntEqRel (..) )


-- This module is almost a verbatim copy of `Data.EqRel` and `Data.HashEqRel`.
-- Sadly, very few things can be shared with those classes, as they require very
-- different types. This one is specialised to Ints and uses `IntSet` and
-- `IntMap`, while the others require more general structures (e.g.,
-- `Map`/`HashMap`)


-- # Types #

-- | Private. The number of elements in an equivalence class.
type ClassSize = Int

-- | Private. A tree node. Every node represents a single element in the set of
-- equivalence classes.
data Node
  = NodeRoot ClassSize
    -- ^ The root of the tree containing the class representative. The size of
    -- the equivalence class is stored so class unification is faster. Namely,
    -- when classes are unified, the smaller class comes to refer to the bigger
    -- class.
  | NodeLink Int
    -- ^ A child node in the tree. It references another node in the tree. When
    -- following the entire chain of links, eventually a root node is reached.

-- | An equivalence relation. It is reflexive, symmetric, and transitive. Note
-- that equivalent elements have to be explicitly added to this relation.
newtype IntEqRel = IntEqRel { treeMap :: IntMap Node }


-- # Construction #

-- | The empty equivalence relation. No distinct elements are equivalent. Every
-- element is only equal to itself (by reflexivity).
empty :: IntEqRel
empty = IntEqRel IntMap.empty

-- | /O(log n)/. Equate two elements in the equivalence relation. This unifies
-- the equivalence classes both elements are members of.
equate :: Int -> Int -> IntEqRel -> IntEqRel
equate a b r =
  let ((reprA, aSize), r2) = mapFst (fromMaybe (a, 1)) (representativeWithSize a r)
      ((reprB, bSize), r3) = mapFst (fromMaybe (b, 1)) (representativeWithSize b r2)
  in
  if reprA == reprB then
    r3 -- 'a' and 'b' are already equal
  else if aSize < bSize then
    -- Make the small set point to the big set
    IntEqRel $ IntMap.insert reprA (NodeLink b) $ IntMap.insert reprB (NodeRoot (aSize + bSize)) $ treeMap r3
  else
    -- Make the small set point to the big set
    IntEqRel $ IntMap.insert reprB (NodeLink a) $ IntMap.insert reprA (NodeRoot (aSize + bSize)) $ treeMap r3

-- | /O(m log (n+m))/. Equate all provided elements in the equivalence relation.
-- This unifies the equivalence classes of all provided elements. Note that /m/
-- represents the number of inserted elements, whereas /n/ is the number of
-- elements already in the relation.
equateAll :: [Int] -> IntEqRel -> IntEqRel
equateAll xs r = foldr ($) r $ zipWith equate xs (safeTail xs)

-- | /O(n log n)/. Collapses the internal tree representation, which eliminates
-- all indirections from the tree. Future queries can discard the updated
-- relation without loss of performance; provided it is not updated afterwards.
collapse :: IntEqRel -> IntEqRel
collapse eqrel = foldr (\a -> snd . representative a) eqrel (IntMap.keys $ treeMap eqrel)


-- # Query #

-- | /O(log n)/. Returns 'True' iff two elements are equal in the
-- provided equivalence class.
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
areEq :: Int -> Int -> IntEqRel -> (Bool, IntEqRel)
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
eqClass :: Int -> IntEqRel -> (IntSet, IntEqRel)
eqClass a r@(IntEqRel m) =
  case representativeWithSize a r of
    -- When 'a' has no representative in the tree, it has simply not been added
    -- to the tree. In that case, 'a' is only equal to itself (by refexivity).
    (Nothing, r2) -> (IntSet.singleton a, r2)
    (Just (aRepr, _), r2) ->
      -- This collapses all paths in the entire tree
      mapFst IntSet.fromAscList $ foldr (step aRepr) ([a], r2) (IntMap.keys m)
  where
  -- | Adds an element to the list if its representative equals the provided
  -- representative. Otherwise it is not added. In either case the updated
  -- (with collapsed paths) equivalence is returned as second tuple element.
  step :: Int -> Int -> ([Int], IntEqRel) -> ([Int], IntEqRel)
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
eqClasses :: IntEqRel -> ([IntSet], IntEqRel)
eqClasses r =
  let (classes, r') = eqClassesWithRepresentative r
  in (map snd classes, r')

-- | /O(n log n)/. Returns the representative of the element's equivalence
-- class.
--
-- The second element in the tuple is the updated input equivalence relation.
-- It may safely be discarded w.r.t. correctness. However, it is advised to
-- use it going forward instead of the input relation w.r.t. performance.
-- Internal data is updated (paths are collapsed) which ensures the amortized
-- time complexity.
representative :: Int -> IntEqRel -> (Int, IntEqRel)
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
combine :: IntEqRel -> IntEqRel -> IntEqRel
combine a b =
  -- Loop over all equivalence classes in 'b' and insert them into 'a'
  foldr (equateAll . IntSet.toList) a (fst $ eqClasses b)


-- # Conversion #

-- | /O(n log n)/. Constructs a equivalence relation from the given list of
-- equivalence classes.
--
-- Example:
--
-- > let rel = fromList [[1,2],[4,5,6],[7]]
-- > fst (areEquivalent 4 6 rel) = True
-- > fst (areEquivalent 6 7 rel) = False
fromList :: [[Int]] -> IntEqRel
fromList = foldr equateAll empty

-- | /O(n log n)/. Produces a frozen version of the equivalence relation:
-- `FrozenIntEqRel`. While that representation cannot be modified, it can be
-- queried significantly quicker.
--
-- Note that this function produces the fully collapsed equivalence relation as
-- the second element of the returned tuple.
freeze :: IntEqRel -> (FrozenIntEqRel, IntEqRel)
freeze rel =
  let (allClasses, rel') = eqClassesWithRepresentative rel
      elements = foldr insertClass IntMap.empty allClasses
  in (FrozenIntEqRel elements allClasses, rel')
  where
  insertClass :: (Int, IntSet) -> IntMap (Int, IntSet) -> IntMap (Int, IntSet)
  insertClass (r,s) m = foldr (\x -> IntMap.insert x (r,s)) m (IntSet.toList s)


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
representativeWithSize :: Int -> IntEqRel -> (Maybe (Int, ClassSize), IntEqRel)
representativeWithSize a r@(IntEqRel m) =
  case a `IntMap.lookup` m of
    Nothing -> (Nothing, r)
    Just (NodeRoot n) -> (Just (a, n), r)
    Just (NodeLink aNext)   ->
      case representativeWithSize aNext r of
        -- Collapse the path
        (Just (repr, n), r2) -> (Just (repr, n), IntEqRel $ IntMap.insert a (NodeLink repr) $ treeMap r2)
        -- A link node points nowhere. Tree construction ensures this does not
        -- happen.
        (Nothing, _)  -> error "Invalid Tree"

-- | Private. /O(n log n)/. Returns all equivalence classes; each with their
-- representative. Note that this collapses the entire tree. The relation with
-- its entire tree collapsed is returned as the second element in the tuple.
eqClassesWithRepresentative :: IntEqRel -> ([(Int, IntSet)], IntEqRel)
eqClassesWithRepresentative r@(IntEqRel m) =
  mapFst IntMap.assocs $ foldr step (IntMap.empty, r) $ IntMap.keys m
  where
  -- Adds a single element to the result. Keeps track of:
  -- * A map whose keys are representatives, and whose values are the values in
  --   its equivalence class.
  -- * The input equivalence class whose internal representation may collapse
  --   along each step. It's contents do not change, though.
  step :: Int -> (IntMap IntSet, IntEqRel) -> (IntMap IntSet, IntEqRel)
  step a (res, r) =
    let (aRepr, r2) = representative a r
    in (insertToSet aRepr a res, r2)

-- | Applies the function to the first element of the tuple.
mapFst :: ( a -> c ) -> ( a, b ) -> ( c, b )
mapFst f (a, b) = (f a, b)

-- | Prelude's 'tail' function throws an error when given the empty list. This
-- implementation simply returns the empty list as the tail of the empty list.
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

-- | Inserts a single element into a map containing sets of elements.
insertToSet :: Int -> Int -> IntMap IntSet -> IntMap IntSet
insertToSet k v =
  IntMap.alter (Just . IntSet.insert v . fromMaybe IntSet.empty) k


-- # Class instances

instance Show IntEqRel where
  show = show . map IntSet.toList . fst . eqClasses

instance Semigroup IntEqRel where
  (<>) = combine

instance Monoid IntEqRel where
  mempty = empty
