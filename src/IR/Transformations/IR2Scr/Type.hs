{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Type where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr

instance ToScryptTransformable IType' (Maybe Type) where
  _toScrypt (Just (ElementaryType IR.Bool)) = Just Scr.Bool
  _toScrypt (Just (ElementaryType IR.Int)) = Just Scr.Int
  _toScrypt (Just (ElementaryType IR.Bytes)) = Just Scr.Bytes
  _toScrypt (Just (ElementaryType IR.Address)) = Just $ Scr.SubBytes Scr.Ripemd160
  _toScrypt t = error $ "Type `" ++ show t ++ "` not implemented in scrypt"

