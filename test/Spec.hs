import Lib
import Test.HUnit

main :: IO Counts
main = runTestTT tests

tests = TestList [TestLabel "testArraySubtraction" testArraySubtraction]

testArraySubtraction =
  TestCase $ assertEqual
  "Array Subtraction"
  (arraySubtraction [1,2,3,4,5,6,7,8,9] [9,4])
  [1,2,3,5,6,7,8]
