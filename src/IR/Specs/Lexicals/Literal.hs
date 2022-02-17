module IR.Specs.Lexicals.Literal where

import Data.Word

data ILiteral 
  = BoolLiteral Bool
  | IntLiteral {isHex :: Bool, intVal :: Integer}
  | BytesLiteral [Word8]
  | StringLiteral String
  deriving (Eq, Show, Ord)