module Puzzle where

toPuzzle :: [[Int]] -> Maybe [[Int]]
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
