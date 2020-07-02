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
    , ("Puzzle: solveCell returns correct value."
      , testSolveCell)
    , ("Puzzle: solveCell returns nothing."
      , testSolveCellReturnsNothing)
    , ("Puzzle: getboxOrigin returns correct row column pair."
      , testGetBoxOrigin)
    , ("Puzzle: getSolvables returns list of solvable cells"
      , testGetSolvables)
    --, ("Puzzle: solvePuzzle successfully solves the puzzle."
    --  ,testSolvePuzzle)
    , ("Puzzle: testFillCell successfully populates cell."
      , testFillCell)
    , ("Puzzle: testFillCell successfully populates cell."
      ,testSolvePuzzle2)
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



testSolvePuzzle =
  let
    solvedPuzzle =
      [
        [5,3,4,6,7,8,9,1,2]
        , [6,7,2,1,9,5,3,4,8]
        , [1,9,8,3,4,2,5,6,7]
        , [8,5,9,7,6,1,4,2,3]
        , [4,2,6,8,5,3,7,9,1]
        , [7,1,3,9,2,4,8,5,6]
        , [9,6,1,5,3,7,2,8,4]
        , [2,8,7,4,1,9,6,3,5]
        , [3,4,5,2,8,6,1,7,9]
      ]
  in
  TestCase $ assertEqual
  "Failed to solve the puzzle."
  solvedPuzzle
  (solvePuzzle validPuzzleSeed)

testSolvePuzzle2 =
  let
    validSeed =
      [
        [3,0,0,8,0,1,0,0,2]
        , [2,0,1,0,3,0,6,0,4]
        , [0,0,0,2,0,4,0,0,0]
        , [8,0,9,0,0,0,1,0,6]
        , [0,6,0,0,0,0,0,5,0]
        , [7,0,2,0,0,0,4,0,9]
        , [0,0,0,5,0,9,0,0,0]
        , [9,0,4,0,8,0,7,0,5]
        , [6,0,0,1,0,7,0,0,3]
      ]
    solvedPuzzle =
      [
        [3,4,6,8,9,1,5,7,2]
        , [2,9,1,7,3,5,6,8,4]
        , [5,7,8,2,6,4,3,9,1]
        , [8,5,9,4,7,3,1,2,6]
        , [4,6,3,9,1,2,8,5,7]
        , [7,1,2,6,5,8,4,3,9]
        , [1,3,7,5,4,9,2,6,8]
        , [9,2,4,3,8,6,7,1,5]
        , [6,8,5,1,2,7,9,4,3]
      ]
  in
  TestCase $ assertEqual
  "Failed to solve the puzzle."
  solvedPuzzle
  (solvePuzzle validSeed)

testFillCell =
  let
    changedPuzzle =
      [
        [5,3,0, 0,7,0, 0,0,0]
        , [6,0,0, 1,9,5, 0,0,0]
        , [0,9,8, 0,0,2, 0,6,0]
        --
        , [8,0,0, 0,6,0, 0,0,3]
        , [4,0,0, 8,0,3, 0,0,1]
        , [7,0,0, 0,2,0, 0,0,6]
        --
        , [0,6,0, 0,0,0, 2,8,0]
        , [0,0,0, 4,1,9, 0,0,5]
        , [0,0,0, 0,8,0, 0,7,9]
      ]
  in
  TestCase $ assertEqual
  "Unsuccessfully attempted to fill a cell value."
  changedPuzzle
  (fillCell (2,5) 2 validPuzzleSeed)


testGetSolvables =
  TestCase $ assertEqual
  "Returned something other than the cells that are currently solvable."
  [(4,4,5),(6,5,7),(6,8,4),(7,7,3)]
  (getSolvables validPuzzleSeed)

testGetBoxOrigin =
  TestCase $ assertEqual
  "Returned something other than the box origin"
  (3,6)
  (getBoxOrigin (4, 7))

testGetCell =
  TestCase $ assertEqual
  "Returned something other than the correct cell."
  (getCell (2,7) validPuzzleSeed)
  6

testSolveCell =
  TestCase $ assertEqual
  "Returned something other than 4."
  (solveCell (8,8) validPuzzleSeed)
  (Just 4)

testSolveCellReturnsNothing =
  TestCase $ assertEqual
  "Should have returned Nothing."
  (solveCell (4,6) validPuzzleSeed)
  Nothing

testGetBox =
  TestCase $ assertEqual
  "Retrieved something other than the correct box."
  (sort [0,0,0,0,0,0,1,3,6])
  (sort $ getBox (5,8) validPuzzleSeed)

testGetBoxCount =
  let
    elementCount = length $ getBox (2,7) validPuzzleSeed
  in
  TestCase $ assertBool
  ("This should have returned an array of exactly 9 integers, but instead returned " ++ show elementCount ++ " elements.")
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
