module IR.Specs.Function where

import IR.Specs.Lexical
import IR.Specs.Variable
import IR.Specs.Type
import IR.Specs.Statement

data IFunction = Function
  { funcName :: IIdentifier,
    funcParams :: IParamList,
    funcBody :: IBlock,
    funcReturn :: IType,
    funcVisibility :: IVisibility
  }
  deriving (Show, Eq, Ord)

data IConstructor = Constructor
  { ctorParams :: IParamList,
    ctorBody :: IBlock
  }
  deriving (Show, Eq, Ord)