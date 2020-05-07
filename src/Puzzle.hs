module Puzzle where

import Data.List

type Puzzle = [[Int]]
type Coordinate = (Int, Int)
type Row = [Int]
type Column = [Int]
type Box = [Int]
type Cell = Int

toPuzzle :: Puzzle -> Maybe Puzzle
toPuzzle puzzle =
  let

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

    hasValidRowCount :: Puzzle -> Bool
    hasValidRowCount puzzle =
      length puzzle == 9

    hasValidColumnCount :: Puzzle -> Bool
    hasValidColumnCount puzzle =
      let
        listOfLengths = map length puzzle
        listOfNonNine = filter (/=9) listOfLengths
        validity = not $ length listOfNonNine > 0
      in
        validity

    isValid  :: Puzzle -> Bool
    isValid puzzle =
      hasValidRowCount puzzle
      && hasValidColumnCount puzzle
      && elementsInValidRange puzzle

    maybePuzzle = if' (isValid puzzle) (Just puzzle) Nothing
  in
  maybePuzzle

solveCell :: Coordinate -> Puzzle -> Maybe Cell
solveCell (x, y) arr =
  let
    row = getRow y arr
    column = getColumn x arr
    box = getBox (x, y) arr
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

getBox :: Coordinate -> Puzzle -> Box
getBox a puzzle =
  let
    originX = fst $ getBoxOrigin a
    originY = snd $ getBoxOrigin a
    endingX = originX + 2
    endingY = originY + 2
    xRange = [originX..endingX]
    yRange = [originY..endingY]

    getBoxOrigin :: Coordinate -> Coordinate
    getBoxOrigin coordinate =
      let
        x = fst coordinate
        originX = axisBoxOrigin x
        y = snd coordinate
        originY = axisBoxOrigin y

        axisBoxOrigin :: Int -> Int
        axisBoxOrigin coord =
          if' (coord `mod` 3 == 0)
            coord
            (axisBoxOrigin (coord - 1))
      in
        (originX, originY)

    getCell :: Coordinate -> Puzzle -> Int
    getCell a puzzle =
      let
        x = fst a
        y = snd a
        row = puzzle !! y
        cell = row !! x
      in
        cell

    result = [getCell (x,y) puzzle | x <- xRange, y <- yRange]
  in
  result

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

--For Repl convenience.

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
