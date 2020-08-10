
module Data.HashEqRelTest
  ( tests
  ) where

-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import           Test.Tasty.QuickCheck ( testProperty )
import qualified Data.HashSet as HashSet
import           Data.HashSet ( HashSet )
import           Data.Hashable ( Hashable )
-- Local library imports
import qualified Data.HashEqRel as HashEqRel
import           Data.HashEqRel ( HashEqRel, areEquivalent, empty, equate, equateAll, equivalenceClass, equivalenceClasses, combine )


-- TODO: Make tests exhaustive. They are not exhaustive yet by far. Only unit
-- tests are currently present which eliminate many obvious errors.
--
-- These tests are almost a verbatim copy of the tests in `Data.EqRelTest`.
-- Sadly, little code can be trivially shared between the two as they require
-- different traits (i.e., `Ord` vs `Eq`+`Hashable`)


tests :: TestTree
tests =
  testGroup "HashEqRelTest"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################

-- Two sample equivalence relations
r,q :: HashEqRel String
r = equate "A" "B" $ equate "X" "Y" $ equateAll ["P", "Q", "R"] $ equate "B" "Z" empty
q = equate "C" "D" $ equateAll ["R","V","Z"] empty

unitTests :: [TestTree]
unitTests =
  [ testCase "equality stored" $
      assertBool [] (fst $ areEquivalent "A" "B" $ equate "A" "B" empty)
  , testCase "reflexive" $
      assertBool [] (fst $ areEquivalent "A" "A" empty)
  , testCase "symmetric" $
      assertBool [] (fst $ areEquivalent "B" "A" $ equate "A" "B" empty)
  , testCase "transitive" $
      assertBool [] (fst $ areEquivalent "C" "E" $ equate "C" "D" $ equate "D" "E" empty)
  , testCase "no magic 1" $
      assertBool [] (not $ fst $ areEquivalent "A" "B" empty)
  , testCase "no magic 2" $
      assertBool [] (not $ fst $ areEquivalent "A" "B" $ equateAll ["C", "D", "X", "Z"] empty)
  , testCase "no magic transitive 1" $
      assertBool [] (not $ fst $ areEquivalent "D" "A" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "no magic transitive 2" $
      assertBool [] (not $ fst $ areEquivalent "A" "D" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "no magic transitive 3" $
      assertBool [] (not $ fst $ areEquivalent "C" "A" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "no magic transitive 4" $
      assertBool [] (not $ fst $ areEquivalent "A" "C" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "no magic transitive 5" $
      assertBool [] (not $ fst $ areEquivalent "D" "B" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "no magic transitive 6" $
      assertBool [] (not $ fst $ areEquivalent "B" "D" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "no magic transitive 7" $
      assertBool [] (not $ fst $ areEquivalent "C" "B" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "no magic transitive 8" $
      assertBool [] (not $ fst $ areEquivalent "B" "C" $ equate "C" "D" $ equate "A" "B" empty)
  , testCase "class extraction" $
      assertEqual [] (HashSet.fromList ["A","B","Z"]) (fst $ equivalenceClass "Z" r)
  , testCase "classes extraction" $
      assertEqual [] (toSetSet [["A","B","Z"], ["X","Y"], ["P","Q","R"]]) (HashSet.fromList $ fst $ equivalenceClasses r)
  , testCase "combine" $
      assertEqual [] (toSetSet [["C","D"], ["X","Y"], ["A","B","P","Q","R","V","Z"]]) (HashSet.fromList $ fst $ equivalenceClasses $ combine r q)
  ]

-- | Helper. Converts a list of lists to a set of sets.
toSetSet :: (Eq a, Hashable a) => [[a]] -> HashSet (HashSet a)
toSetSet = HashSet.fromList . map HashSet.fromList


-- #############################################################################
-- # # # # # # # # # # # # # # # Property Tests  # # # # # # # # # # # # # # # #
-- #############################################################################


-- TODO
quickcheckTests :: [TestTree]
quickcheckTests = []
