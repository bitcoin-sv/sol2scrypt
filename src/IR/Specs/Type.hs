module IR.Specs.Type where

import IR.Specs.Lexical
import IR.Specs.Expression


data IType
  = ElementaryType IElementaryTypeName
  | BuiltinType String
  | UserDefinedType IUserDefinedTypeName
  | Array IType IExpr
  deriving (Show, Eq, Ord)

data IElementaryTypeName 
  = Bool
  | Int
  | Bytes
  | String
  | Address
  | Any
  deriving (Show, Eq, Ord)

newtype IUserDefinedTypeName = UserDefinedTypeName [IIdentifier] deriving (Show, Eq, Ord)