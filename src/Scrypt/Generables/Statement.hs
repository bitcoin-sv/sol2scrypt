{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Statement where

import Scrypt.Generables.Base
import Scrypt.Generables.Expression
import Scrypt.Generables.Variable 

import Scrypt.Spec as Scr

instance Generable (Maybe (Scr.Statement a)) where
  genCode = maybe "" genCode

instance Generable (Scr.Statement a) where
  genCode (ExprStmt expr _) = genCode expr ++ ";"
  genCode (Assign lhs expr _) = genCode lhs ++ " = " ++ genCode expr ++ ";"
  genCode (Declare declare expr _) = genCode declare ++ " = " ++ genCode expr ++ ";"
  genCode (ReturnStmt e _) = "return " ++ genCode e ++ ";"
  genCode (Require e _) = "require(" ++ genCode e ++ ");"
  genCode (Block stmts _) = "{" ++ unwords (map genCode stmts) ++ "}"
  genCode _ = error "unimplemented show scrypt expr"
