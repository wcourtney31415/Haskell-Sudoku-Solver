module TestArraySubtraction (arraySubtractionTests) where

import ArraySubtraction
import Test.HUnit
import CustomTestHelpers

arraySubtractionTests = prepTests "TestArraySubtraction" tests

tests =
  [
    ("Array Subtraction" ,
      testArraySubtraction)
  , ("Array Subtraction: empty target list",
      testArraySubtractionBlankTargets)
  , ("Array Subtraction: Empty target list, and duplicate elements.",
      arrSubBlankTargetsDupElem)
  , ("Array Subtraction: Empty Source List.",
      arrSubBlankSourceArr)
  ]

-- Tests --

testArraySubtraction =
  TestCase $ assertEqual
  "Array Subtraction"
  (arraySubtraction [1,2,3,4,5,6,7,8,9] [9,4])
  [1,2,3,5,6,7,8]

testArraySubtractionBlankTargets =
  let
    myList = [1,2,3,4,5,6,7,8,9]
  in
  TestCase $ assertEqual
    "Empty target list"
    (arraySubtraction myList [])
    myList

arrSubBlankTargetsDupElem =
  let
    myList = [1,2,3,4,5,6,7,8,9]
    listWithDups = [1,2,3,4,5,6,7,8,9,2,4,6]
  in
  TestCase $ assertEqual
    "Empty Target List"
    (arraySubtraction myList [] /= listWithDups)
    True

arrSubBlankSourceArr =
  TestCase $ assertEqual
  "Empty Source List"
  (arraySubtraction [] [4,6])
  []
