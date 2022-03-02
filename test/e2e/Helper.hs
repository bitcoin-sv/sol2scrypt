{-# LANGUAGE ScopedTypeVariables #-}

module Helper where

import Solidity as Sol
import Text.Parsec.Pos

firstLineSR :: (Int, Int) -> SourceRange
firstLineSR (startCol, endCol) = SourceRange (firstLinePos startCol) (firstLinePos endCol)
  where
    firstLinePos = newPos "" 1


newSR :: (Int, Int) -> (Int, Int) -> SourceRange
newSR (line, col) (line', col') = SourceRange (newPos "" line col) (newPos "" line' col')