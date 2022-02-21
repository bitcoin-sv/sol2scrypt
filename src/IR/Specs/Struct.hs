module IR.Specs.Struct where

import IR.Specs.Variable

data IStruct = Struct
  { structName :: String,
    structFields :: [IParam]
  }
  deriving (Show, Eq, Ord)