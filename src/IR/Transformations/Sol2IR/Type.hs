{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Type where

import IR.Transformations.Base
import Solidity.Spec as Sol
import IR.Spec as IR

-- from TypeName to IType'
instance ToIRTransformable TypeName IType' where
  _toIR (TypeNameElementaryTypeName Sol.BoolType) = return $ Just $ ElementaryType Bool
  _toIR (TypeNameElementaryTypeName (Sol.IntType _)) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (Sol.UintType _)) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (Sol.BytesType _)) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName Sol.ByteType) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName Sol.StringType) = return $ Just $ ElementaryType String
  _toIR (TypeNameElementaryTypeName Sol.AddressType) = return $ Just $ ElementaryType Address
  _toIR (TypeNameElementaryTypeName Sol.VarType) = return $ Just $ ElementaryType Any
  _toIR _ = return Nothing -- ignore those which can not be transformed
