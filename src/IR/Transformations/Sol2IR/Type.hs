{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Type where

import IR.Transformations.Base
import Solidity.Spec as Sol
import IR.Spec as IR
import Utils
import IR.Transformations.Sol2IR.Expression ()

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
  _toIR (TypeNameArrayTypeName t e) = do
    t' <- _toIR t
    sub <- _toIR e
    let arr = flip Array sub
    return $ arr <$> t'
  _toIR t = error $ "unsupported type `" ++ headWord (show t) ++ "`"
