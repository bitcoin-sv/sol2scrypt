module IR.Specs.Variable where

import IR.Specs.Lexical
import IR.Specs.Type
import IR.Specs.Expression

data IParam = Param
  { paramType :: IType,
    paramName :: IIdentifier
  }
  deriving (Show, Eq, Ord)

newtype IParamList = ParamList [IParam] deriving (Show, Eq, Ord)

data IStateVariable = StateVariable
  {
    stateVarName :: IIdentifier,
    stateVarType :: IType,
    stateInitialValue :: Maybe IExpr
  }
  deriving (Show, Eq, Ord)
