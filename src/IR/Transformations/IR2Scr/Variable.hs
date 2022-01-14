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


instance ToScryptTransformable IStateVariable (Scr.Param Ann) where
  _toScrypt (IR.StateVariable name varType vis Nothing False) = Scr.Param (TypeAnn (_toScrypt varType) nil) (_toScrypt name) (Const False) Nothing (_toScrypt vis) (IsStateProp True) nil
  _toScrypt t = error $ "IStateVariable to (Scr.Param Ann) `" ++ show t ++ "` not implemented in scrypt"


instance ToScryptTransformable IStateVariable  (Scr.Static Ann) where
  _toScrypt (IR.StateVariable name varType vis (Just expr) True) = let param = Scr.Param (TypeAnn (_toScrypt varType) nil) (_toScrypt name) (Const True) Nothing (_toScrypt vis) (IsStateProp False) nil
      in Scr.Static param (_toScrypt expr) nil
  _toScrypt t = error $ "IStateVariable to Maybe (Scr.Static Ann) `" ++ show t ++ "` not implemented in scrypt"
