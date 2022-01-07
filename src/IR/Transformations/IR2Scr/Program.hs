{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Program where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.IR2Scr.Contract ()
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IProgram' (Maybe (Scr.Program Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IProgram (Scr.Program Ann) where
  _toScrypt (IR.Program _ contracts _) = Scr.Program [] [] [] (map _toScrypt contracts) nil