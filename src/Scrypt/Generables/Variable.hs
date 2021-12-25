{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scrypt.Generables.Variable where

import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Spec as Scr
import Utils

instance Generable (Maybe (Scr.Param Ann)) where
  genCode = maybe "" genCode

instance Generable (Scr.Param a) where
  genCode (Param (TypeAnn pt _) pn _ _ _ (IsStateProp True) _) = do
    "@state " ++ genCode pt ++ " " ++ genCode pn ++ ";"
  genCode (Param (TypeAnn pt _) pn _ _ _ _ _) = do
    genCode pt ++ " " ++ genCode pn