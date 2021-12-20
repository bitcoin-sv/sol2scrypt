{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Type where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr

instance ToScryptTransformable IType' (Maybe Type) where
  _toScrypt Nothing = Nothing
  _toScrypt (Just t) = Just $ toScryptType t

toScryptType :: IType -> Type
toScryptType (ElementaryType IR.Bool) = Scr.Bool
toScryptType (ElementaryType IR.Int) = Scr.Int
toScryptType (ElementaryType IR.Bytes) = Scr.Bytes
toScryptType (ElementaryType IR.Any) = Scr.Any
toScryptType (ElementaryType IR.Address) = Scr.SubBytes Scr.Ripemd160
toScryptType t = error $ "IType `" ++ show t ++ "` not implemented in scrypt"