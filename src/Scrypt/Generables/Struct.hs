{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrypt.Generables.Struct where

import Data.List (intercalate)
import Scrypt.Generables.Base
import Scrypt.Generables.Variable ()
import Scrypt.Spec as Scr

instance Generable (Maybe (Scr.Struct a)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.Struct a) where
  genCode (Scr.Struct sn fields _) = do
    let firstLine = "struct " ++ sn ++ " {"
        lastLine = "\n}"
    incIndent
    fields' <- mapM fieldDecalre fields
    decIndent
    return $ firstLine ++ intercalate "" fields' ++ lastLine
    where
      fieldDecalre p = do
        p' <- genCode p
        withIndent $ p' ++ ";"
