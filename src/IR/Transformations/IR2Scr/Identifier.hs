{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Identifier where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IIdentifier (Expr Ann) where
  _toScrypt (Identifier i) = Var v False nil
                                        where
                                          v = map (\c -> if c == '$' then '_' else c) i

instance ToScryptTransformable IIdentifier' (Maybe (Expr Ann)) where
  _toScrypt = (<$>) _toScrypt