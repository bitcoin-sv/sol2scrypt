{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scrypt.Generables.Variable where

import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Spec as Scr
import Utils

instance Generable (Maybe (Scr.Param Ann)) where
  genCode = maybe (return "") genCode


instance Generable Scr.Visibility where
  genCode Public = return "public"
  genCode Private = return "private"
  genCode Default = return ""


instance Generable (Scr.Param a) where
  genCode (Param (TypeAnn pt _) pn _ _ vis (IsStateProp True) _) = do
    vis' <- genCode vis
    pt' <- genCode pt
    pn' <- genCode pn
    pstr <- withIndent $ (if vis /= Default then vis' ++ " " else "") ++  pt' ++ " " ++ pn' ++ ";"
    withIndent $ "@state" ++ pstr
  genCode (Param (TypeAnn pt _) pn _ _ _ _ _) = do
    pt' <- genCode pt
    pn' <- genCode pn
    return $ pt' ++ " " ++ pn'