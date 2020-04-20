module TestPuzzle (puzzleTests) where

import Puzzle
import Test.HUnit
import CustomTestHelpers

puzzleTests = prepTests "TestPuzzle.hs" tests

tests = [
  ("No Negative Cells", seedWithNegative)
  , ("Invalid Row Count", incorrectNumOfRows)
  , ("Invalid Cell Count", correctCellCount)
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

incorrectNumOfRows =
  let
    invalidPuzzleSeed =
      [
        [5,3,0, 0,7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,0, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
      ]
  in
  TestCase $ assertEqual
  "Invalid row count"
  ((toPuzzle invalidPuzzleSeed) == invalidPuzzleSeed)
  False

seedWithNegative =
  let
    invalidPuzzleSeed =
      [
          [5,3,0, 0,-7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,0, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
        , [4,0,0, 8,0,3, 0,-4,1]
        , [7,0,0, 0,2,0, 0,0,6]
        --
        , [0,6,0, 0,0,0, 2,8,0]
        , [0,-2,0, 4,1,9, 0,0,5]
        , [0,0,0, 0,8,0, 0,7,9]
      ]
  in
  TestCase $ assertEqual
  "No Negative Cells"
  ((toPuzzle invalidPuzzleSeed) == invalidPuzzleSeed)
  False



correctCellCount =
  let
    invalidPuzzleSeed =
      [
        [5,3,0, 0,7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,0, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
        , [4,0,0, 8,0,3, 0,0,1,5,5,5]
        , [7,0,0, 0,2,0, 0,0,6]
        --
        , [0,6,0, 0,0,0, 2,8,0]
        , [0,0,0, 4,1,9, 0,0,5]
        , [0,0,0, 0,8,0, 0,7,9]
        , [0,0,0, 0,8,0, 0,7,9]
      ]
  in
  TestCase $ assertEqual
  "Incorrect Number of Cells"
  ((toPuzzle invalidPuzzleSeed) == invalidPuzzleSeed)
  False
