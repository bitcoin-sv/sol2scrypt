{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module IR.Transformations.IR2Scr.Variable where

import IR.Transformations.Base
import IR.Transformations.IR2Scr.Identifier ()
import IR.Transformations.IR2Scr.Expression ()
import IR.Transformations.IR2Scr.Type ()
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IParam' (Maybe (Scr.Param Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IParam (Scr.Param Ann) where
  _toScrypt (IR.Param pt pn) = Scr.Param (TypeAnn (_toScrypt pt) nil) (_toScrypt pn) (Const False) Nothing Scr.Default (IsStateProp False) nil


instance ToScryptTransformable IVisibility Scr.Visibility where
  _toScrypt = read . show


instance ToScryptTransformable IProperty (Scr.Param Ann) where
  _toScrypt (IR.Property name varType vis _ (IsConst isConst) (IsStatic False) (IsState isState)) = Scr.Param (TypeAnn (_toScrypt varType) nil) (_toScrypt name) (Const isConst) Nothing (_toScrypt vis) (IsStateProp isState) nil
  _toScrypt t = error $ "IProperty to (Scr.Param Ann) `" ++ show t ++ "` not implemented in scrypt"


instance ToScryptTransformable IProperty  (Scr.Static Ann) where
  _toScrypt (IR.Property name varType vis (Just expr) (IsConst isConst) (IsStatic True) (IsState False)) = let param = Scr.Param (TypeAnn (_toScrypt varType) nil) (_toScrypt name) (Const isConst) Nothing (_toScrypt vis) (IsStateProp False) nil
      in Scr.Static param (_toScrypt expr) nil
  _toScrypt t = error $ "IProperty to Maybe (Scr.Static Ann) `" ++ show t ++ "` not implemented in scrypt"
