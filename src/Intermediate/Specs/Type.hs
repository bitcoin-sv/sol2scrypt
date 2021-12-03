module Intermediate.Specs.Type where

data IType
  = ITypeBool
  | ITypeInt
  | ITypeBytes
  | ITypeString
  | ITypeAddress
  deriving (Show, Eq)