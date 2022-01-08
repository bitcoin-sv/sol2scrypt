{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrypt.Generables.Statement where

import Data.List (intercalate)
import Scrypt.Generables.Base
import Scrypt.Generables.Expression
import Scrypt.Generables.Variable
import Control.Monad
import Scrypt.Spec as Scr

instance Generable (Maybe (Scr.Statement a)) where
  genCode = maybe (return "") genCode

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
  genCode (If e trueBranch maybeFalseBranch _) = do
    e' <- genCode e
    -- tbInd: true branch need increase indent
    let tbInd = case trueBranch of
          Scr.Block _ _ -> False
          _ -> True
    -- fbInd: false branch need increase indent
    let (fbExists, fbInd) = case maybeFalseBranch of
          Nothing -> (False, True)
          Just falseBranch -> case falseBranch of
            Scr.Block _ _ -> (True, False)
            Scr.If {} -> (True, False)
            _ -> (True, True)

    ifLine <- withIndent $ "if (" ++ e' ++ ")"
    trueBranch' <- genBranch tbInd $ Just trueBranch
    elseLine <- if fbExists then withIndent "else" else return ""
    falseBranch' <- genBranch fbInd maybeFalseBranch

    return $ ifLine ++ trueBranch' ++ elseLine ++ falseBranch'
  genCode _ = error "unimplemented show scrypt expr"

genBranch :: Bool -> Maybe (Statement a) -> CodeGenerator String
genBranch needInd branch = do
  when needInd incIndent
  branch' <- genCode branch
  let branch'' = if needInd then branch' else " " ++ removeIndent branch'
  when needInd decIndent
  return branch''
