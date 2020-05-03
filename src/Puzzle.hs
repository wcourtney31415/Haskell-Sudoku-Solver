module Puzzle where

toPuzzle :: [[Int]] -> Maybe [[Int]]
toPuzzle arr =
  let
    hasValidRowCount arr =
      if length arr /= 9 then
        False
      else
        True
    isValid =
      hasValidRowCount
  in
  if isValid arr then
    Just arr
  else
    Nothing
