{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Scrypt.Generables.Variable where

import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Spec as Scr
import Utils

instance Generable (Maybe (Scr.Param Ann)) where
  genCode = maybe "" genCode

instance Generable (Scr.Param a) where
  genCode (Param (TypeAnn pt _) pn _ _ _ _ _) = genCode pt ++ " " ++ genCode pn
  genCode _ = error "unimplemented show scrypt expr"