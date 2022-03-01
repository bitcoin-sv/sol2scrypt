{-# LANGUAGE ScopedTypeVariables #-}

module Helper where

import Solidity as Sol
import Text.Parsec.Pos

firstLineSR :: (Int, Int) -> SourceRange
firstLineSR (startCol, endCol) = SourceRange (firstLinePos startCol) (firstLinePos endCol)
  where
    firstLinePos = newPos "" 1