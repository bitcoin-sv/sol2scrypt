{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Intermediate.Transformations.IntM2Scr.Type where

import Intermediate.Transformations.Base
import Intermediate.Spec as IntM
import Scrypt.Spec as Scr

instance ToScryptTransformable IType' (Maybe Type) where
  _toScrypt (Just (ElementaryType IntM.Bool)) = Just Scr.Bool
  _toScrypt (Just (ElementaryType IntM.Int)) = Just Scr.Int
  _toScrypt (Just (ElementaryType IntM.Bytes)) = Just Scr.Bytes
  _toScrypt (Just (ElementaryType IntM.Address)) = Just $ Scr.SubBytes Scr.PubKey
  _toScrypt t = error $ "Type `" ++ show t ++ "` not implemented in scrypt"

