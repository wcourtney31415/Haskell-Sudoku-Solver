module TestArraySubtraction where

import ArraySubtraction
import Test.HUnit

arraySubtractionTests = [
  TestLabel "Array Subtraction" testArraySubtraction
  , TestLabel
    "Array Subtraction: empty target list"
    testArraySubtractionBlankTargets
  , TestLabel
    "Array Subtraction: Empty target list, and duplicate elements."
    arrSubBlankTargetsDupElem
  , TestLabel
    "Array Subtraction: Empty Source List."
    arrSubBlankSourceArr
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
