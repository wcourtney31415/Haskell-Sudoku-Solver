module Puzzle where

toPuzzle :: [[Int]] -> Maybe [[Int]]
toPuzzle arr =
  let
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
      hasValidRowCount arr && hasValidColumnCount arr
  in
  if isValid arr then
    Just arr
  else
    Nothing
