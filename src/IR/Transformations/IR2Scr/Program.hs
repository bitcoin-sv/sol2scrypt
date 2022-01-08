{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Program where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.IR2Scr.Contract ()
import Scrypt.Spec as Scr
import Utils


instance ToScryptTransformable IImportDirective' (Maybe (Scr.ImportPath Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IR.IImportDirective (Scr.ImportPath Ann) where
  _toScrypt (IR.ImportDirective path) = Scr.ImportPath path nil


instance ToScryptTransformable IProgram' (Maybe (Scr.Program Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IProgram (Scr.Program Ann) where
  _toScrypt (IR.Program imports contracts _) = Scr.Program (map _toScrypt imports) [] [] (map _toScrypt contracts) nil