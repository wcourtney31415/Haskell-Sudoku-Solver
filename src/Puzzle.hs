module Puzzle where

import PuzzleValidity
import MyTypes


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

if' :: Bool -> a -> a -> a
if' True x _  = x
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
