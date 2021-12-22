{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Identifier where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr

instance ToScryptTransformable IIdentifier (Expr IExpr) where
  _toScrypt e@(Identifier i) = Var v False $ IdentifierExpr e
                                        where
                                          v = map (\c -> if c == '$' then '_' else c) i

instance ToScryptTransformable IIdentifier' (Maybe (Expr IExpr)) where
  _toScrypt Nothing = Nothing
  _toScrypt (Just e) = Just $ _toScrypt e