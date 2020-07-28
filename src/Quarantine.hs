module Quarantine where

import MyTypes



-- getSolvables :: Puzzle -> [Solvable]
-- getSolvables puzzle =
--   let
--     cellIsZero row column =
--       0 == getCell (row, column) puzzle
--     range = [0..8]
--     handleIt (Just a) = a
--     handleIt Nothing  = 0
--     getAnswerOrZero row column = (row,column, handleIt $ solveCell (row, column) puzzle)
--   in
--   [getAnswerOrZero r c | r <- range, c <- range
--   , Data.Maybe.isJust (solveCell (r, c) puzzle), cellIsZero r c]
