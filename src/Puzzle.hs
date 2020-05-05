module Puzzle where

type Puzzle = [[Int]]
type Coordinate = (Int, Int)

toPuzzle :: Puzzle -> Maybe Puzzle
toPuzzle arr =
  let

    elementsInValidRange arr =
      let
        listOfElements = foldl (++) [] arr
        isInRange x = x `elem` [0..9]
        validElements = filter isInRange listOfElements
        elementCount = length listOfElements
        validElementCount = length validElements
        isValid =
          if validElementCount < elementCount then
            False
          else
            True
      in
        isValid

    hasValidRowCount arr =
      if length arr /= 9 then
        False
      else
        True

    hasValidColumnCount arr =
      let
        listOfLengths = map length arr
        listOfNonNine = filter (/=9) listOfLengths
      in
        if length listOfNonNine > 0 then
          False
        else
          True

    isValid arr =
      hasValidRowCount arr && hasValidColumnCount arr && elementsInValidRange arr

  in
  if isValid arr then
    Just arr
  else
    Nothing

getRow x arr = arr !! x

getColumn x arr =
  let
    column = map (!!x) arr
  in
    column



getBox :: Coordinate -> Puzzle -> [Int]
getBox a arr =
  let
    originX = fst $ getBoxOrigin a
    originY = snd $ getBoxOrigin a
    endingX = originX + 2
    endingY = originY + 2
    xRange = [originX..endingX]
    yRange = [originY..endingY]

    --Internal Functions
    getBoxOrigin :: Coordinate -> Coordinate
    getBoxOrigin coordinate =
      let
        axisBoxOrigin a =
          if a `mod` 3 == 0 then
            a
          else
            axisBoxOrigin (a - 1)
        x = fst coordinate
        originX = axisBoxOrigin x
        y = snd coordinate
        originY = axisBoxOrigin y
      in
        (originX,originY)

    getCell :: Coordinate -> Puzzle -> Int
    getCell a arr =
      let
        x = fst a
        y = snd a
        row = arr !! y
        cell = row !! x
      in
        cell
  in
  [getCell (x,y) arr | x <- xRange, y <- yRange]


--For Repl convenience.
{-
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
