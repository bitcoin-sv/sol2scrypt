module IR.Specs.Variable where

import IR.Specs.Expression
import IR.Specs.Lexical
import IR.Specs.Type

data IParam = Param
  { paramType :: IType,
    paramName :: IIdentifier
  }
  deriving (Show, Eq, Ord)

newtype IParamList = ParamList [IParam] deriving (Show, Eq, Ord)

data IVisibility
  = Public
  | Private
  | Default
  deriving (Show, Eq, Ord)

data IProperty = Property
  { propName :: IIdentifier,
    propType :: IType,
    propVisibility :: IVisibility,
    propInitialValue :: Maybe IExpression,
    propIsConstant :: IsConst,
    propIsStatic :: IsStatic,
    propIsState :: IsState
  }
  deriving (Show, Eq, Ord)

newtype IsConst = IsConst {unConst :: Bool} deriving (Show, Eq, Ord)

newtype IsStatic = IsStatic {unStatic :: Bool} deriving (Show, Eq, Ord)

newtype IsState = IsState {unState :: Bool} deriving (Show, Eq, Ord)
