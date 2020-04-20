module CustomTestHelpers where

import Test.HUnit

toTestLabel fileName tup =
  let
    title = fileName ++ " : " ++ (fst tup)
    test = snd tup
  in
    TestLabel title test
