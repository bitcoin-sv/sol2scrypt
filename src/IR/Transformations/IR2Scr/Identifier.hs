{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Identifier where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IIdentifier' (Maybe (NameAnn Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IIdentifier (NameAnn Ann) where
  _toScrypt (Identifier i) = NameAnn (transformReserved n) nil
                                        where
                                          n = map (\c -> if c == '$' then '_' else c) i
                                          transformReserved s = if s `elem` reservedKeywords  
                                                                    then "userDefined_" ++ s
                                                                    else s
