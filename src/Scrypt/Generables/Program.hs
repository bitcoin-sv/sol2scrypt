{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Program where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr
import Utils

instance Generable (Maybe (Scr.Program Ann)) where
  genCode Nothing = return ""
  genCode (Just t) = genCode t

instance Generable (Scr.Program Ann) where
  genCode _ = return ""
