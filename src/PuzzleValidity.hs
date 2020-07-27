module PuzzleValidity (isValid) where

import MyTypes

elementsInValidRange :: PrePuzzle -> Bool
elementsInValidRange puzzle =
  let
    listOfElements = concat puzzle
    isInRange x = x `elem` [0..9]
    validElements = filter isInRange listOfElements
    elementCount = length listOfElements
    validElementCount = length validElements
    validity = (validElementCount >= elementCount)
  in
  validity

hasValidColumnCount :: PrePuzzle -> Bool
hasValidColumnCount puzzle =
  let
    listOfLengths = map length puzzle
    listOfNonNine = filter (/=9) listOfLengths
    validity = null listOfNonNine
  in
    validity

hasValidRowCount :: PrePuzzle -> Bool
hasValidRowCount puzzle =
  length puzzle == 9

isValid  :: PrePuzzle -> Bool
isValid puzzle =
  hasValidRowCount puzzle
  && hasValidColumnCount puzzle
  && elementsInValidRange puzzle
