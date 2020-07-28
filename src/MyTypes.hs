module MyTypes where

data CellValue =
  Unsolved
  | Solved Int
  deriving (Eq, Show)

type PrePuzzle = [[Int]]
type Puzzle = [[CellValue]]
type Row = [CellValue]
type Column = [CellValue]
type Box = [CellValue]
type RowColumnPair = (Int, Int)

if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y
