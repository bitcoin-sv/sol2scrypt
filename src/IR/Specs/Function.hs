module IR.Specs.Function where

import IR.Specs.Lexical
import IR.Specs.Statement
import IR.Specs.Type

data IFunction = Function
  { funcName :: IIdentifier,
    funcParams :: IParamList,
    funcBody :: IBlock,
    funcReturn :: IParamList,
    funcVisibility :: IVisibility,
    funcStatic :: IStatic
  }
  deriving (Show, Eq, Ord)

data IConstructor = Constructor
  { ctorParams :: IParamList,
    ctorBody :: IBlock
  }
  deriving (Show, Eq, Ord)

data IParam = Param
  { paramType :: IType,
    paramName :: IIdentifier
  }
  deriving (Show, Eq, Ord)

newtype IParamList = ParamList [IParam] deriving (Show, Eq, Ord)

data IVisibility
  = Public
  | Private
  | Internal
  | External
  deriving (Show, Eq, Ord)

newtype IStatic = Static {unIStatic :: Bool} deriving (Show, Eq, Ord)
