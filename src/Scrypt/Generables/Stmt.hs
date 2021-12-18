{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Stmt where

import Scrypt.Generables.Base
import Scrypt.Generables.Expr ()
import Scrypt.Spec as Scr

instance Generable (Maybe (Scr.Statement a)) where
  genCode (Just (ExprStmt expr _)) = genCode (Just expr) ++ ";"
  genCode _ = error "unimplemented show scrypt expr"
