module TestPuzzle (puzzleTests) where

import Puzzle
import Test.HUnit
import CustomTestHelpers

puzzleTests = prepTests "TestPuzzle.hs" tests

tests =
  [
    ("Test making a puzzle"
      ,testToPuzzle)
    , ("Test excessive rows returns nothing"
      , testExcessiveRows)
    , ("Test that nothing is returned on bad row count"
      , testReturnNothingOnBadRowCount)
    , ("Test that nothing is returned on bad column count"
      , testReturnNothingOnBadColumnCount)
  ]

validPuzzleSeed =
  [
    [5,3,0, 0,7,0, 0,0,0]
    , [6,0,0, 1,9,5, 0,0,0]
    , [0,9,8, 0,0,0, 0,6,0]
    --
    , [8,0,0, 0,6,0, 0,0,3]
    , [4,0,0, 8,0,3, 0,0,1]
    , [7,0,0, 0,2,0, 0,0,6]
    --
    , [0,6,0, 0,0,0, 2,8,0]
    , [0,0,0, 4,1,9, 0,0,5]
    , [0,0,0, 0,8,0, 0,7,9]
  ]

testToPuzzle =
  TestCase $ assertEqual
    "Test if valid input returns a puzzle"
    (toPuzzle validPuzzleSeed)
    (Just validPuzzleSeed)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

testExcessiveRows =
  let
    excessiveRows =
      [
        [5,3,0, 0,7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,0, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
        , [4,0,0, 8,0,3, 0,0,1]
        , [7,0,0, 0,2,0, 0,0,6]
        --
        , [0,6,0, 0,0,0, 2,8,0]
        , [0,0,0, 4,1,9, 0,0,5]
        , [0,0,0, 0,8,0, 0,7,9]
        , [0,0,0, 0,8,0, 0,7,9]
      ]
  in
  TestCase $ assertBool
  "Test that you can't have a puzzle with more than 9 rows"
  (isNothing $ toPuzzle excessiveRows)

testReturnNothingOnBadRowCount =
  let
    badRowCount =
      [
        [5,3,0, 0,7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,0, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
        , [4,0,0, 8,0,3, 0,0,1]
        , [7,0,0, 0,2,0, 0,0,6]
        --
        , [0,6,0, 0,0,0, 2,8,0]
        , [0,0,0, 4,1,9, 0,0,5]
        , [0,0,0, 0,8,0, 0,7,9]
        , [0,0,0, 0,8,0, 0,7,9]
      ]
  in
  TestCase $ assertBool
  "Invalid row count should have returned Nothing."
  (isNothing $ toPuzzle badRowCount)


testReturnNothingOnBadColumnCount =
  let
    badColumnCount=
      [
        [5,3,0, 0,7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,0, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
        , [4,0,0, 8,0,3, 0,0,1]
        , [7,0,0, 0,2,0, 0,0,6]
        --
        , [0,6,0, 0,0,0, 2,8,0]
        , [0,0,0, 4,1,9, 0,0,5, 5]
        , [0,0,0, 0,8,0, 0,7,9]
      ]
  in
  TestCase $ assertBool
  "Invalid column count should have returned Nothing."
  (isNothing $ toPuzzle badColumnCount)
