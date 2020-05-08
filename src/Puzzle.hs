module Puzzle where

import Data.List

type Puzzle = [[Int]]
type RowColumnPair = (Int, Int)
type Row = [Int]
type Column = [Int]
type Box = [Int]
type Cell = Int

elementsInValidRange :: Puzzle -> Bool
elementsInValidRange puzzle =
  let
  listOfElements = foldl (++) [] puzzle
  isInRange x = x `elem` [0..9]
  validElements = filter isInRange listOfElements
  elementCount = length listOfElements
  validElementCount = length validElements
  validity = not $ validElementCount < elementCount
  in
  validity

hasValidColumnCount :: Puzzle -> Bool
hasValidColumnCount puzzle =
  let
    listOfLengths = map length puzzle
    listOfNonNine = filter (/=9) listOfLengths
    validity = not $ length listOfNonNine > 0
  in
    validity

hasValidRowCount :: Puzzle -> Bool
hasValidRowCount puzzle =
  length puzzle == 9

isValid  :: Puzzle -> Bool
isValid puzzle =
  hasValidRowCount puzzle
  && hasValidColumnCount puzzle
  && elementsInValidRange puzzle

toPuzzle :: Puzzle -> Maybe Puzzle
toPuzzle puzzle =
  let
    maybePuzzle = if' (isValid puzzle) (Just puzzle) Nothing
  in
  maybePuzzle

solveCell :: RowColumnPair -> Puzzle -> Maybe Cell
solveCell (rowNumber, columnNumber) arr =
  let
    row = getRow rowNumber arr
    column = getColumn columnNumber arr
    box = getBox (rowNumber, columnNumber) arr
    possibilities = getPossibilities row column box
    solvable = if' (length possibilities == 1) True False
    maybeValue = if' solvable (Just $ possibilities !! 0) Nothing
  in
    maybeValue

getPossibilities :: Row -> Column -> Box -> [Int]
getPossibilities row column box =
  [1..9] \\ (row ++ column ++ box)

getRow :: Int -> Puzzle -> Row
getRow x puzzle = puzzle !! x

getColumn :: Int -> Puzzle -> Column
getColumn x puzzle = map (!!x) puzzle

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

getCell :: RowColumnPair -> Puzzle -> Int
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

getSolvables arr =
  let
  cellIsZero row column =
    0 == getCell (row, column) arr
  range = [0..8]
  makeTuple row column = (row,column, solveCell (row, column) arr)
  in
  [makeTuple r c | r <- range, c <- range, solveCell (r, c) arr /= Nothing, cellIsZero r c]

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

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
