{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scrypt.Generables.Type where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

instance Generable Scr.Type where
  genCode = show
