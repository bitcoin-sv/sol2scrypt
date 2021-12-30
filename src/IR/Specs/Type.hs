module IR.Specs.Type where

import IR.Specs.Lexical


data IType
  = ElementaryType IElementaryTypeName
  | BuiltinType String
  | UserDefinedType IUserDefinedTypeName
  | Array IType Int
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