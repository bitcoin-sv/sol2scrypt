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
  _toIntermediate (TypeNameElementaryTypeName Sol.BoolType) = return $ Just $ ElementaryType Bool
  _toIntermediate (TypeNameElementaryTypeName (Sol.IntType _)) = return $ Just $ ElementaryType Int
  _toIntermediate (TypeNameElementaryTypeName (UintType _)) = return $ Just $ ElementaryType Int
  _toIntermediate (TypeNameElementaryTypeName (Sol.BytesType _)) = return $ Just $ ElementaryType Bytes
  _toIntermediate (TypeNameElementaryTypeName ByteType) = return $ Just $ ElementaryType Bytes
  _toIntermediate (TypeNameElementaryTypeName Sol.StringType) = return $ Just $ ElementaryType String
  _toIntermediate _ = return Nothing
