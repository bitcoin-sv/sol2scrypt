{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Empty where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

instance Generable (Maybe Scr.Empty) where
  genCode _ = ""