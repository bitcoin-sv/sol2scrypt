{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrypt.Generables.Function where

import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Generables.Stmt ()
import Scrypt.Generables.Variable ()

import Scrypt.Spec as Scr
import Data.List (intercalate)
import Utils
import qualified Data.Text as T

instance Generable (Maybe (Scr.Function Ann)) where
  genCode = maybe "" genCode

instance Generable (Scr.Function Ann) where
  genCode (Scr.Function fn ps (TypeAnn rt _) (RegularBody stmts _ _) vis stc _) = T.unpack $ T.strip $ T.pack c
    where
      c = genCode vis ++ if unStatic stc then " static " else ""
          ++ " function " ++ genCode fn
          ++ "("
          ++ intercalate ", " (map genCode ps)
          ++ ")"
          ++ (if vis == Public then "" else " : " ++ genCode (Just rt))
          ++ " { "
          ++ unwords (map (genCode . Just ) stmts)
          ++ " } "
  genCode _ = ""

instance Generable Scr.Visibility where
  genCode Public = " public"
  genCode Private = " private"
  genCode Default = ""