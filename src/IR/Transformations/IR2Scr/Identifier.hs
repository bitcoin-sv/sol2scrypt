{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Identifier where

import IR.Spec as IR
import IR.Transformations.Base
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IIdentifier' (Maybe (NameAnn Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IIdentifier (NameAnn Ann) where
  _toScrypt (ReservedId i) = NameAnn n nil
    where
      n =
        if i `elem` reservedKeywords
          then i
          else error $ "`" ++ i ++ "` is not in reservedKeywords"
  _toScrypt (Identifier i) = NameAnn (transformReserved n) nil
    where
      n = map (\c -> if c == '$' then '_' else c) i
      transformReserved s =
        if s `elem` reservedKeywords
          then "userDefined_" ++ s
          else s

instance ToScryptTransformable IIdentifier (Expr Ann) where
  _toScrypt i = let (NameAnn v _) :: NameAnn Ann = _toScrypt i in Var v False nil
