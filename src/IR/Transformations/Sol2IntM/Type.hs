{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IntM.Type where

import IR.Transformations.Base
import Solidity.Spec as Sol
import IR.Spec as IR

-- from TypeName to IType'
instance ToIRTransformable TypeName IType' where
  _toIR (TypeNameElementaryTypeName Sol.BoolType) = return $ Just $ ElementaryType Bool
  _toIR (TypeNameElementaryTypeName (Sol.IntType _)) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (UintType _)) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (Sol.BytesType _)) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName ByteType) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName Sol.StringType) = return $ Just $ ElementaryType String
  _toIR _ = return Nothing
