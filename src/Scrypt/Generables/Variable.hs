{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scrypt.Generables.Variable where

import Scrypt.Generables.Base
import Scrypt.Generables.Expression ()
import Scrypt.Generables.Type ()
import Scrypt.Spec as Scr
import Utils

instance Generable (Maybe (Scr.Param Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Maybe (Scr.Param Ann, Bool)) where
  genCode = maybe (return "") genCode


instance Generable Scr.Visibility where
  genCode Public = return "public"
  genCode Private = return "private"
  genCode Default = return ""

instance Generable (Scr.Param a, Bool) where
  -- for non-state properties
  genCode (Param (TypeAnn pt _) pn (Const isConst) _ vis (IsStateProp False) _, True) = do
    vis' <- genCode vis
    pt' <- genCode pt
    pn' <- genCode pn
    let constStr = if isConst then "const " else ""
        visStr = if vis /= Default then vis' ++ " " else ""
    withIndent $ visStr ++ constStr ++ pt' ++ " " ++ pn' ++ ";"
  genCode (p, _) = genCode p

instance Generable (Scr.Param a) where
  -- for state properties
  genCode (Param (TypeAnn pt _) pn (Const isConst) _ vis (IsStateProp True) _) = do
    vis' <- genCode vis
    pt' <- genCode pt
    pn' <- genCode pn
    let constStr = if isConst then "const " else ""
        visStr = if vis /= Default then vis' ++ " " else ""
    pStr' <- withIndent $ visStr ++ constStr ++ pt' ++ " " ++ pn' ++ ";"
    withIndent $ "@state" ++ pStr'
  -- for common paremeters, such as function param or struct fields
  genCode (Param (TypeAnn pt _) pn _ _ _ _ _) = do
    pt' <- genCode pt
    pn' <- genCode pn
    return $ pt' ++ " " ++ pn'

instance Generable (Maybe (Scr.Static Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.Static a) where
  genCode (Static param expr _) = do
    param' <- genCode param
    expr' <- genCode expr
    withIndent $ "static const " ++ param' ++ " = " ++ expr' ++ ";"