{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module IR.Transformations.IR2Scr.Contract where

import IR.Transformations.Base
import IR.Transformations.IR2Scr.Identifier ()
import IR.Transformations.IR2Scr.Type ()
import IR.Transformations.IR2Scr.Variable ()
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IContractBodyElement' (Maybe (Scr.Param Ann)) where
  _toScrypt = (<$>) _toScrypt


instance ToScryptTransformable IR.IContractBodyElement (Scr.Param Ann) where
  _toScrypt (IR.StateVariableDeclaration declare) = _toScrypt declare
  