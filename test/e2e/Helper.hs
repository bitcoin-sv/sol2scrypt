{-# LANGUAGE ScopedTypeVariables #-}

module Helper where

import Solidity as Sol
import Text.Parsec.Pos

-- the first line's source range
firstLineSR :: (Int, Int) -> SourceRange
firstLineSR (startCol, endCol) = newSR (1, startCol) (1, endCol)

newSR :: (Int, Int) -> (Int, Int) -> SourceRange
newSR (line, col) (line', col') = SourceRange (newPos "" line col) (newPos "" line' col')
