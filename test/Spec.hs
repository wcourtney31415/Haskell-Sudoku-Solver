import Test.HUnit
import TestArraySubtraction

main :: IO Counts
main = runTestTT tests

tests = TestList arraySubtractionTests
