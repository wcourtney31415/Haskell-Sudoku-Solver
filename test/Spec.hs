import Test.HUnit
import TestPuzzle

main :: IO Counts
main = runTestTT tests

tests = TestList puzzleTests
