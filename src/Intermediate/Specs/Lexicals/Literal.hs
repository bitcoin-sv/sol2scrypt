module Intermediate.Specs.Lexicals.Literal where

import Data.Word

data ILiteral 
  = BoolLiteral Bool
  | IntLiteral {isHex :: Bool, intVal :: Integer}
  | BytesLiteral [Word8]
  deriving (Eq, Show, Ord)