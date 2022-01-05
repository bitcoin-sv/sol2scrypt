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

  genCode (If e ifstmts elsestmt _) = do
    e' <- genCode e

    elsestmt' <- genCode elsestmt
    let isBlock = case ifstmts of
          Scr.Block _ _ -> True
          _ -> False
    ifPart <-
      if isBlock
        then do
          ifstmts' <- genCode ifstmts
          return $ " " ++ removeIndent ifstmts'
        else do
          incIndent
          ifstmts' <- genCode ifstmts
          decIndent
          return ifstmts'
    elsePart <- if elsestmt' == "" then withEmptyIndent else withIndent $ "\nelse " ++ removeIndent elsestmt'

    withIndent $ "if(" ++ e' ++ ")" ++ ifPart ++ elsePart
  genCode _ = error "unimplemented show scrypt expr"
