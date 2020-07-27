module MyTypes where

data CellValue =
  Unsolved
  | Solved Int
  deriving (Eq, Show)

type PrePuzzle = [[Int]]
type Puzzle = [[CellValue]]
