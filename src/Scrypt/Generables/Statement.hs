{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Statement where

import Scrypt.Generables.Base
import Scrypt.Generables.Expression
import Scrypt.Generables.Variable 

import Scrypt.Spec as Scr
import Data.List (intercalate)

instance Generable (Maybe (Scr.Statement a)) where
  genCode Nothing = return ""
  genCode (Just t) = genCode t

instance Generable (Scr.Statement a) where
  genCode (ExprStmt expr _) = do
    expr' <- genCode expr
    withIndent $ expr' ++ ";"
  genCode (Assign lhs expr _) = do
    lhs' <- genCode lhs
    expr' <- genCode expr
    withIndent $ lhs' ++ " = " ++ expr' ++ ";"
  genCode (Declare var expr _) = do
    var' <- genCode var
    expr' <- genCode expr
    withIndent $ var' ++ " = " ++ expr' ++ ";"
  genCode (ReturnStmt e _) = do
    e' <- genCode e
    withIndent $ "return " ++ e' ++ ";"
  genCode (Require e _) = do
    e' <- genCode e
    withIndent $ "require(" ++ e' ++ ");"
  genCode (Block stmts _) = do
    openBrace <- withIndent "{"
    incIndent
    stmts' <- mapM genCode stmts
    decIndent
    closeBrace <- withIndent "}"
    return $ openBrace ++ intercalate "" stmts' ++ closeBrace
  genCode EmptyStmt = return ""
  genCode _ = error "unimplemented show scrypt expr"
