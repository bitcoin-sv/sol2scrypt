{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scrypt.Generables.Variable where

import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Spec as Scr
import Utils

instance Generable (Maybe (Scr.Param Ann)) where
  genCode = maybe "" genCode


instance Generable Scr.Visibility where
  genCode Public = "public"
  genCode Private = "private"
  genCode Default = ""


instance Generable (Scr.Param a) where
  genCode (Param (TypeAnn pt _) pn _ _ vis (IsStateProp True) _) = do
    let visstr  = if vis == Default then "" else genCode vis ++ " "
    "@state " ++ visstr ++ genCode pt ++ " " ++ genCode pn ++ ";"
  genCode (Param (TypeAnn pt _) pn _ _ _ _ _) = do
    genCode pt ++ " " ++ genCode pn