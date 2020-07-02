module CustomTestHelpers where

import Test.HUnit

toTestLabel fileName tup =
  let
    title = fileName ++ " : " ++ fst tup
    test = snd tup
  in
    TestLabel title test

prepTests fileName =
  map (toTestLabel fileName)


assertUnequal text valueA valueB =
  assertEqual text (valueA /= valueB) True
