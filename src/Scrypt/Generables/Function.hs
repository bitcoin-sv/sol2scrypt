{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrypt.Generables.Function where

import Data.List (intercalate)
import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Generables.Statement ()
import Scrypt.Generables.Variable ()
import Scrypt.Spec as Scr
import Utils

instance Generable (Maybe (Scr.Function Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.Function Ann) where
  genCode (Scr.Function fn ps (TypeAnn rt _) (RegularBody stmts _ _) vis stc _) = do
    vis' <- genCode vis
    fn' <- genCode fn
    ps' <- mapM genCode ps
    rt' <- genCode (Just rt)
    firstLine <-
      withIndent $
        trim $
          vis' ++ (if unStatic stc then " static " else "")
            ++ " function "
            ++ fn'
            ++ "("
            ++ intercalate ", " ps'
            ++ ")"
            ++ (if vis == Public then "" else " : " ++ rt')
            ++ " {"

    incIndent
    stmts' <- mapM genCode stmts
    decIndent
    lastLine <- withIndent "}"

    return $ firstLine  ++ intercalate "" stmts' ++ lastLine
  genCode _ = return ""
