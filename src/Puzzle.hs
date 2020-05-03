module Puzzle where

toPuzzle :: [[Int]] -> Maybe [[Int]]
toPuzzle arr =
  if length arr > 9 then
    Nothing
  else
    Just arr
