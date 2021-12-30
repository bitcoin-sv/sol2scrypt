{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Type where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr
import IR.Transformations.IR2Scr.Expression ()

instance ToScryptTransformable IType' (Maybe Type) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IType Type where
  _toScrypt (ElementaryType IR.Bool) = Scr.Bool
  _toScrypt (ElementaryType IR.Int) = Scr.Int
  _toScrypt (ElementaryType IR.Bytes) = Scr.Bytes
  _toScrypt (ElementaryType IR.Any) = Scr.Any
  _toScrypt (ElementaryType IR.Address) = Scr.SubBytes Scr.Ripemd160
  _toScrypt (BuiltinType "SigHashPreimage") = Scr.SubBytes Scr.SigHashPreimage 
  _toScrypt (IR.Array t e) = Scr.Array (_toScrypt t) (Scr.CTCConst $ toInteger e)
  _toScrypt t = error $ "IType `" ++ show t ++ "` not implemented in scrypt"