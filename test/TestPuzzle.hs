module TestPuzzle (puzzleTests) where

import Puzzle
import Test.HUnit
import CustomTestHelpers
import Data.List
import MyTypes

puzzleTests = prepTests "TestPuzzle.hs" tests

tests =
  [
     ("Puzzle: arrToPuzzle successfully creates puzzle."
      , testArrToPuzzle)
     , ("Puzzle: Get back Nothing if prepuzzle has too many rows."
       , testExcessiveRows)
     , ("Puzzle: Get back Nothing if prepuzzle has bad row count."
       , testReturnNothingOnBadRowCount)
     , ("Puzzle: Get back Nothing if prepuzzle has bad column count."
       , testReturnNothingOnBadColumnCount)
     , ("Puzzle: Get back Nothing if prepuzzle has bad column count."
       , testReturnNothingOnBadColumnCount)
     , ("Puzzle: Get back Nothing if prepuzzle contains num other than [0..9]."
       , testReturnNothingOnBadColumnCount)
  ]

validPuzzleSeed =
  [
    [   5,3,0,  0,7,0,  0,0,0 ]
    , [ 6,0,0,  1,9,5,  0,0,0 ]
    , [ 0,9,8,  0,0,0,  0,6,0 ]
    -------------------------
    , [ 8,0,0,  0,6,0,  0,0,3 ]
    , [ 4,0,0,  8,0,3,  0,0,1 ]
    , [ 7,0,0,  0,2,0,  0,0,6 ]
    -------------------------
    , [ 0,6,0,  0,0,0,  2,8,0 ]
    , [ 0,0,0,  4,1,9,  0,0,5 ]
    , [ 0,0,0,  0,8,0,  0,7,9 ]
  ]



testArrToPuzzle =
  let
    answer = Just
      [
        [   Solved 5, Solved 3, Unsolved,      Unsolved, Solved 7, Unsolved,      Unsolved, Unsolved, Unsolved ]
        , [ Solved 6, Unsolved, Unsolved,      Solved 1, Solved 9, Solved 5,      Unsolved, Unsolved, Unsolved ]
        , [ Unsolved, Solved 9, Solved 8,      Unsolved, Unsolved, Unsolved,      Unsolved, Solved 6, Unsolved ]
        --------------------------------------------------------------------------------------------------------------------------
        , [ Solved 8, Unsolved, Unsolved,      Unsolved, Solved 6, Unsolved,      Unsolved, Unsolved, Solved 3 ]
        , [ Solved 4, Unsolved, Unsolved,      Solved 8, Unsolved, Solved 3,      Unsolved, Unsolved, Solved 1 ]
        , [ Solved 7, Unsolved, Unsolved,      Unsolved, Solved 2, Unsolved,      Unsolved, Unsolved, Solved 6 ]
        --------------------------------------------------------------------------------------------------------------------------
        , [ Unsolved, Solved 6, Unsolved,      Unsolved, Unsolved, Unsolved,      Solved 2, Solved 8, Unsolved ]
        , [ Unsolved, Unsolved, Unsolved,      Solved 4, Solved 1, Solved 9,      Unsolved, Unsolved, Solved 5 ]
        , [ Unsolved, Unsolved, Unsolved,      Unsolved, Solved 8, Unsolved,      Unsolved, Solved 7, Solved 9 ]
      ]
    puzzle = arrToPuzzle validPuzzleSeed
  in
  TestCase $ assertEqual
  "Failed to create puzzle."
  answer
  puzzle

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
  (isNothing $ arrToPuzzle excessiveRows)

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
  (isNothing $ arrToPuzzle badRowCount)


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
  (isNothing $ arrToPuzzle badColumnCount)

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
  (isNothing $ arrToPuzzle elementsOutOfBounds)
