{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Empty where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr

instance ToScryptTransformable IEmpty Scr.Empty where
  _toScrypt _ = Scr.Empty
  