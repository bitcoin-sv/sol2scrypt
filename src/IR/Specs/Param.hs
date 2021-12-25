module IR.Specs.Param where

import IR.Specs.Lexical
import IR.Specs.Type

data IParam = Param
  { paramType :: IType,
    paramName :: IIdentifier
  }
  deriving (Show, Eq, Ord)

newtype IParamList = ParamList [IParam] deriving (Show, Eq, Ord)
