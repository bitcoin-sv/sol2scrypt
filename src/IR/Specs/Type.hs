module IR.Specs.Type where

import IR.Specs.Lexical

data IType
  = ElementaryType IElementaryTypeName
  | UserDefinedType IUserDefinedTypeName
  deriving (Show, Eq, Ord)

data IElementaryTypeName 
  = Bool
  | Int
  | Bytes
  | String
  | Address
  | Any
  deriving (Show, Eq, Ord)

newtype IUserDefinedTypeName = IUserDefinedTypeName [IIdentifier] deriving (Show, Eq, Ord)