{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module IR.Transformations.IR2Scr.Variable where

import IR.Transformations.Base
import IR.Transformations.IR2Scr.Identifier ()
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
  _toScrypt (IR.StateVariable name varType vis Nothing) = Scr.Param (TypeAnn (_toScrypt varType) nil) (_toScrypt name) (Const False) Nothing (_toScrypt vis) (IsStateProp True) nil
  _toScrypt t = error $ "IType `" ++ show t ++ "` not implemented in scrypt"
