import Test.HUnit
import TestArraySubtraction
import TestPuzzle

main :: IO Counts
main = runTestTT tests

tests = TestList $
  arraySubtractionTests
  ++ puzzleTests
