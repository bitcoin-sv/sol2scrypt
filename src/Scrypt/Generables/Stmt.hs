{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Stmt where

import Scrypt.Generables.Base
import Scrypt.Generables.Expr ()
import Scrypt.Spec as Scr

instance Generable (Maybe (Scr.Statement a)) where
  genCode = maybe "" genCode

instance Generable (Scr.Statement a) where
  genCode (ExprStmt expr _) = genCode expr ++ ";"
  genCode (Assign lhs expr _) = genCode lhs ++ " = " ++ genCode expr ++ ";"
  genCode (Declare declare expr _) = genCode declare ++ " = " ++ genCode expr ++ ";"
  genCode _ = error "unimplemented show scrypt expr"
