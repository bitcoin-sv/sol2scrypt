{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Intermediate.Transformations.Sol2IntM.Type where

import Intermediate.Transformations.Base
import Solidity.Spec as Sol
import Intermediate.Spec as IntM

-- from TypeName to IType'
instance ToIntermediateTransformable TypeName IType' where
  _toIntermediate (TypeNameElementaryTypeName BoolType) = return $ Just ITypeBool
  _toIntermediate (TypeNameElementaryTypeName (IntType _)) = return $ Just ITypeInt
  _toIntermediate (TypeNameElementaryTypeName (UintType _)) = return $ Just ITypeInt
  _toIntermediate (TypeNameElementaryTypeName (BytesType _)) = return $ Just ITypeBytes
  _toIntermediate (TypeNameElementaryTypeName ByteType) = return $ Just ITypeBytes
  _toIntermediate (TypeNameElementaryTypeName StringType) = return $ Just ITypeString
  _toIntermediate _ = return Nothing
