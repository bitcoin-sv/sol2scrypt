{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Struct where

import IR.Transformations.Base
import IR.Transformations.IR2Scr.Identifier ()
import IR.Transformations.IR2Scr.Type ()
import IR.Transformations.IR2Scr.Variable ()
import IR.Transformations.IR2Scr.Statement ()
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IStruct' (Maybe (Struct Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IStruct (Struct Ann) where
  _toScrypt (IR.Struct sn fields) = Scr.Struct sn (map _toScrypt fields) nil