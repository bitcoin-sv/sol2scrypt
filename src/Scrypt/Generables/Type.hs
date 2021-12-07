{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Type where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

instance Generable (Maybe Scr.Type) where
  genCode (Just t) = show t
  genCode _ = ""
