module Puzzle where

import PuzzleValidity
import MyTypes
import Quarantine
import Data.List

arrToPuzzle :: PrePuzzle -> Maybe Puzzle
arrToPuzzle prePuzzle =
  if isValid prePuzzle then
    let
      convertNum num = if' (num == 0) Unsolved (Solved num)
      convertRow = foldl (\acc x -> acc ++ [convertNum x]) []
      puzzle = foldl (\acc x -> acc ++ [convertRow x]) [] prePuzzle
    in
      Just puzzle
  else
    Nothing

getPossibilities :: Row -> Column -> Box -> [Int]
getPossibilities row column box =
  let
    targetedCells = (row ++ column ++ box)

    unwrapSolved acc x =
      case x of
        Unsolved ->
          acc
        Solved val ->
          val : acc

    unavailableNums = foldl unwrapSolved [] targetedCells

    availableNums = ([1..9] :: [Int]) \\ unavailableNums
  in
    availableNums

getRow :: Int -> Puzzle -> Row
getRow x puzzle = puzzle !! x

getColumn :: Int -> Puzzle -> Column
getColumn x = map (!!x)

axisBoxOrigin :: Int -> Int
axisBoxOrigin rowOrColumn =
  if' (rowOrColumn `mod` 3 == 0)
    rowOrColumn
    (axisBoxOrigin (rowOrColumn - 1))

getBoxOrigin :: RowColumnPair -> RowColumnPair
getBoxOrigin (rowNumber, columnNumber) =
  let
    originRow = axisBoxOrigin rowNumber
    originColumn = axisBoxOrigin columnNumber
  in
  (originRow, originColumn)

getCell :: RowColumnPair -> Puzzle -> CellValue
getCell (rowNumber, columnNumber) puzzle =
  let
    row = puzzle !! rowNumber
    cell = row !! columnNumber
  in
  cell

getBox :: RowColumnPair -> Puzzle -> Box
getBox a@(rowNumber, columnNumber) puzzle =
  let
    originRowColumn = getBoxOrigin a
    startRow = fst originRowColumn
    startColumn = snd originRowColumn
    endRow = startRow + 2
    endColumn = startColumn + 2
    rowRange = [startRow..endRow]
    columnRange = [startColumn..endColumn]
    result = [getCell (rowIndex, columnIndex) puzzle | rowIndex <- rowRange, columnIndex <- columnRange]
  in
  result


--For Repl convenience.
{-
vp :: Puzzle
vp =
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
  -}
