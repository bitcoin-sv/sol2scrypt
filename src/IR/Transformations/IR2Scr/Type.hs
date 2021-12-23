{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Type where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr

instance ToScryptTransformable IType' (Maybe Type) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IType Type where
  _toScrypt (ElementaryType IR.Bool) = Scr.Bool
  _toScrypt (ElementaryType IR.Int) = Scr.Int
  _toScrypt (ElementaryType IR.Bytes) = Scr.Bytes
  _toScrypt (ElementaryType IR.Any) = Scr.Any
  _toScrypt (ElementaryType IR.Address) = Scr.SubBytes Scr.Ripemd160
  _toScrypt t = error $ "IType `" ++ show t ++ "` not implemented in scrypt"