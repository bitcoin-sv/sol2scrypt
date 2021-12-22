{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Statement where

import IR.Transformations.Base
import IR.Spec as IR
import IR.Transformations.IR2Scr.Expression ()
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IStatement' (Maybe (Scr.Statement Ann)) where
  _toScrypt (Just (IR.ExprStmt e)) = let e' = _toScrypt (Just e) in flip Scr.ExprStmt nil <$> e'
  _toScrypt e = error $ "_toScrypt for `" ++ show e ++ "` not implemented in scrypt"
