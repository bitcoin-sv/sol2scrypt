module Intermediate.Specs.Type where

import Intermediate.Specs.Lexical

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
  deriving (Show, Eq, Ord)

newtype IUserDefinedTypeName = IUserDefinedTypeName [IIdentifier] deriving (Show, Eq, Ord)