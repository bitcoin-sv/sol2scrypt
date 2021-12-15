module Intermediate.Specs.Function where

import Intermediate.Specs.Lexical
import Intermediate.Specs.Statement
import Intermediate.Specs.Type

data IFunction = IFunction
  { funcName :: IIdentifier,
    funcParams :: IParamList,
    funcBody :: IBlock,
    funcReturn :: IParamList,
    funcVisibility :: IVisibility,
    funcStatic :: IStatic
  }
  deriving (Show, Eq, Ord)

data IConstructor = IConstructor
  { ctorParams :: IParamList,
    ctorBody :: IBlock
  }
  deriving (Show, Eq, Ord)

data IParam = Param
  { paramType :: IType,
    paramName :: IIdentifier
  }
  deriving (Show, Eq, Ord)

newtype IParamList = IParamList [IParam] deriving (Show, Eq, Ord)

data IVisibility
  = Public
  | Private
  | Internal
  | External
  deriving (Show, Eq, Ord)

newtype IStatic = IStatic {unIStatic :: Bool} deriving (Show, Eq, Ord)
