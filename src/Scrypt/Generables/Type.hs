{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Type where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

instance Generable (Maybe Scr.Type) where
  genCode = maybe "" genCode
  
instance Generable Scr.Type where
  genCode Scr.Any = "auto"
  genCode t = show t