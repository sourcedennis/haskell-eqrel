
module Data.IntEqRelTest
  ( tests
  ) where

-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import           Test.Tasty.QuickCheck ( testProperty )
import qualified Data.Set as Set
import           Data.Set ( Set )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- Local library imports
import qualified Data.IntEqRel as IntEqRel
import           Data.IntEqRel
  ( IntEqRel, areEquivalent, empty, equate, equateAll, equivalenceClass
  , equivalenceClasses, combine, fromList
  )


-- TODO: Make tests exhaustive. They are not exhaustive yet by far. Only unit
-- tests are currently present which eliminate many obvious errors.
--
-- These tests are almost a verbatim copy of the tests in `Data.HashEqRelTest`.
-- Sadly, little code can be trivially shared between the two as they require
-- different traits (i.e., `Ord` vs `Eq`+`Hashable`)

tests :: TestTree
tests =
  testGroup "IntEqRel"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################

-- Two sample equivalence relations
r,q :: IntEqRel
r = equate 1 2 $ equate 24 25 $ equateAll [16, 17, 18] $ equate 2 26 empty
q = equate 3 4 $ equateAll [18,22,26] empty

unitTests :: [TestTree]
unitTests =
  [ testCase "equality stored" $
      assertBool [] (fst $ areEquivalent 1 2 $ equate 1 2 empty)
  , testCase "reflexive" $
      assertBool [] (fst $ areEquivalent 1 1 empty)
  , testCase "symmetric" $
      assertBool [] (fst $ areEquivalent 2 1 $ equate 1 2 empty)
  , testCase "transitive" $
      assertBool [] (fst $ areEquivalent 3 5 $ equate 3 4 $ equate 4 5 empty)
  , testCase "no magic 1" $
      assertBool [] (not $ fst $ areEquivalent 1 2 empty)
  , testCase "no magic 2" $
      assertBool [] (not $ fst $ areEquivalent 1 2 $ equateAll [3, 4, 24, 26] empty)
  , testCase "no magic transitive 1" $
      assertBool [] (not $ fst $ areEquivalent 4 1 $ equate 3 4 $ equate 1 2 empty)
  , testCase "no magic transitive 2" $
      assertBool [] (not $ fst $ areEquivalent 1 4 $ equate 3 4 $ equate 1 2 empty)
  , testCase "no magic transitive 3" $
      assertBool [] (not $ fst $ areEquivalent 3 1 $ equate 3 4 $ equate 1 2 empty)
  , testCase "no magic transitive 4" $
      assertBool [] (not $ fst $ areEquivalent 1 3 $ equate 3 4 $ equate 1 2 empty)
  , testCase "no magic transitive 5" $
      assertBool [] (not $ fst $ areEquivalent 4 2 $ equate 3 4 $ equate 1 2 empty)
  , testCase "no magic transitive 6" $
      assertBool [] (not $ fst $ areEquivalent 2 4 $ equate 3 4 $ equate 1 2 empty)
  , testCase "no magic transitive 7" $
      assertBool [] (not $ fst $ areEquivalent 3 2 $ equate 3 4 $ equate 1 2 empty)
  , testCase "no magic transitive 8" $
      assertBool [] (not $ fst $ areEquivalent 2 3 $ equate 3 4 $ equate 1 2 empty)
  , testCase "class extraction" $
      assertEqual [] (IntSet.fromList [1,2,26]) (fst $ equivalenceClass 26 r)
  , testCase "classes extraction" $
      assertEqual [] (toSetSet [[1,2,26], [24,25], [16,17,18]]) (Set.fromList $ fst $ equivalenceClasses r)
  , testCase "combine" $
      assertEqual [] (toSetSet [[3,4], [24,25], [1,2,16,17,18,22,26]]) (Set.fromList $ fst $ equivalenceClasses $ combine r q)
  , testCase "fromList 1" $
      assertBool [] (fst $ areEquivalent 4 6 $ fromList [[1,2],[4,5,6],[7]])
  , testCase "fromList 2" $
      assertBool [] (not $ fst $ areEquivalent 6 7 $ fromList [[1,2],[4,5,6],[7]])
  ]

-- | Helper. Converts a list of lists to a set of sets.
toSetSet :: [[Int]] -> Set IntSet
toSetSet = Set.fromList . map IntSet.fromList


-- #############################################################################
-- # # # # # # # # # # # # # # # Property Tests  # # # # # # # # # # # # # # # #
-- #############################################################################


-- TODO
quickcheckTests :: [TestTree]
quickcheckTests = []
