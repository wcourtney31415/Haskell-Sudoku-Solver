module Quarantine where

import MyTypes
import Data.Maybe
import Data.List


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

solveCell :: RowColumnPair -> Puzzle -> Maybe Solution
solveCell (rowNumber, columnNumber) puzzle =
  let
    row = getRow rowNumber puzzle
    column = getColumn columnNumber puzzle
    box = getBox (rowNumber, columnNumber) puzzle
    possibilities = getPossibilities row column box
    answer =
      if length possibilities == 1 then
        Just $ head possibilities
      else
        Nothing
  in
    answer
