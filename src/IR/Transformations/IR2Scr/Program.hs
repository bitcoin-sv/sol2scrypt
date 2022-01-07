{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Program where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IProgram' (Maybe (Scr.Program Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IProgram (Scr.Program Ann) where
  _toScrypt t = error $ "IProgram `" ++ show t ++ "` not implemented in scrypt"