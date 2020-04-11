module Lib where

import Data.List

arraySubtraction :: [Int] -> [Int] -> [Int]
arraySubtraction array removalArray =
  if removalArray == [] then
    array
  else
    let
      target = head removalArray
      newArray = dropAllOccurances target array
      updatedRemovalArray = tail removalArray
    in
      arraySubtraction newArray updatedRemovalArray

dropAtIndex :: Int -> [Int] -> [Int]
dropAtIndex index arr =
  let
    theSplit = splitAt index arr
    front = fst theSplit
    back =  tail $ snd theSplit
  in
    front ++ back

dropAllOccurances :: Int -> [Int] -> [Int]
dropAllOccurances val arr =
  let
    targets = findIndices (==val) arr
    firstTarget = head targets
    removedFirstOcc = dropAtIndex firstTarget arr
  in
    if targets == [] then
      arr
    else
      dropAllOccurances val removedFirstOcc
