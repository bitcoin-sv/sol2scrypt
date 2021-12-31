{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Type where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

instance Generable (Maybe Scr.Type) where
  genCode Nothing = return ""
  genCode (Just t) = genCode t

instance Generable Scr.Type where
  genCode Scr.Any = return "auto"
  genCode (Scr.Array t ctc) = do
    t' <- genCode t
    return $ t' ++ "[" ++ show ctc ++ "]"
  genCode t = return $ show t
