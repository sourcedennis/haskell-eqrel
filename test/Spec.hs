
-- External library imports
import Test.Tasty       ( defaultMain, testGroup )
import Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
-- Local imports
import qualified Data.EqRelTest as EqRelTest
import qualified Data.HashEqRelTest as HashEqRelTest
import qualified Data.IntEqRelTest as IntEqRelTest


main :: IO ()
main =
  defaultMain $
    testGroup "Modules"
      [ EqRelTest.tests
      , HashEqRelTest.tests
      , IntEqRelTest.tests
      ]
