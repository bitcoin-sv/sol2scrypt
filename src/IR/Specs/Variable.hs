module IR.Specs.Variable where

import IR.Specs.Lexical
import IR.Specs.Type
import IR.Specs.Expression


data IStateVariable = StateVariable
  {
    stateVarName :: IIdentifier,
    stateVarType :: IType,
    stateInitialValue :: Maybe IExpr
  }
  deriving (Show, Eq, Ord)
