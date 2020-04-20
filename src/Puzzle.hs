module Puzzle where



toPuzzle puzzleSeed =
  rowLengthFilter
  $ negativeFilter
  $ puzzleSeed

negativeFilter puzzleSeed =
  let
    flatArr = concat puzzleSeed
    negatives = [x | x <- flatArr, x < 0]
    numOfNegatives = length negatives
    ret =
      if numOfNegatives > 0 then
        error $ "The following invalid negatives were found: " ++ show negatives
      else
        puzzleSeed
  in
    ret

rowLengthFilter puzzleSeed =
  if length puzzleSeed /= 9 then
    error ("There are " ++ (show $ length puzzleSeed) ++ " rows and there should be 9.")
  else
    puzzleSeed
