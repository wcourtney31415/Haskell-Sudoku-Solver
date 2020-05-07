module TestPuzzle (puzzleTests) where

import Puzzle
import Test.HUnit
import CustomTestHelpers
import Data.List

puzzleTests = prepTests "TestPuzzle.hs" tests

tests =
  [
    ("Puzzle Creation: With Valid Input"
      ,testMakePuzzleWithValidInput)
    , ("Puzzle Creation: Input with too many Rows"
      , testExcessiveRows)
    , ("Puzzle Creation: Input with bad row count."
      , testReturnNothingOnBadRowCount)
    , ("Puzzle Creation: Input with bad column count."
      , testReturnNothingOnBadColumnCount)
    , ("Puzzle Creation: Input with elements out of range 0..9"
      , testThatEachElementIsZeroToNine)
    , ("Puzzle: getRow returns the correct value."
      , testGetRow)
    , ("Puzzle: getColumn returns the correct value."
      , testGetColumn)
    , ("Puzzle: getBox returns the correct value."
      , testGetBox)
    , ("Puzzle: getBox returns exactly 9 Ints."
      , testGetBoxCount)
    , ("Puzzle: getPossibilities returns only valid possibilities."
      , testGetPossibilites)
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

testGetBox =
  TestCase $ assertEqual
  "Retrieved something other than the correct box."
  (sort [0,0,0,4,1,9,0,8,0])
  (sort $ getBox (5,8) validPuzzleSeed)

testGetBoxCount =
  let
    elementCount = length $ getBox (2,7) validPuzzleSeed
  in
  TestCase $ assertBool
  ("This should have returned an array of exactly 9 integers, but instead returned " ++ (show elementCount) ++ " elements.")
  (elementCount == 9)

testGetPossibilites =
  TestCase $ assertEqual
  "Failed"
  (getPossibilities [4,6,8] [2,4,3] [3,1])
  [5,7,9]

testGetColumn =
  TestCase $ assertEqual
  "Retrieved something other than the correct column."
  [3,0,9,0,0,0,6,0,0]
  (getColumn 1 validPuzzleSeed)

testGetRow =
  TestCase $ assertEqual
  "Retrieved something other than the correct row."
  [8,0,0, 0,6,0, 0,0,3]
  (getRow 3 validPuzzleSeed)


testMakePuzzleWithValidInput =
  TestCase $ assertEqual
    "Given that the input is valid, it should have returned a puzzle."
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
  "Too many rows should have returned Nothing."
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

testThatEachElementIsZeroToNine =
  let
    elementsOutOfBounds =
      [
        [5,3,0, 0,7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,0, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
        , [4,0,0, 8,0,3, 12,0,1]
        , [7,0,0, 0,2,0, 0,0,6]
        --
        , [0,6,0, 0,0,0, 2,8,0]
        , [0,0,0, 4,1,9, 0,0,5]
        , [0,0,0, 0,8,0, 0,7,9]
      ]
  in
  TestCase $ assertBool
  "Input containing elements not within 0..9 should have returned Nothing."
  (isNothing $ toPuzzle elementsOutOfBounds)
