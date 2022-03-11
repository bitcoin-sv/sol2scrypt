{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Program where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.IR2Scr.Struct ()
import IR.Transformations.IR2Scr.Contract ()
import Scrypt.Spec as Scr
import Utils


instance ToScryptTransformable IProgram' (Maybe (Scr.Program Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IProgram (Scr.Program Ann) where
  _toScrypt (IR.Program imports contracts librarys structs) = Scr.Program imports' [] structs'  (librarys' ++ contracts') nil
    where
      imports' = map _toScrypt imports
      structs' = map _toScrypt structs
      contracts' = map _toScrypt contracts
      librarys' = map _toScrypt librarys


instance ToScryptTransformable IImportDirective' (Maybe (Scr.ImportPath Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IR.IImportDirective (Scr.ImportPath Ann) where
  _toScrypt (IR.ImportDirective path) = Scr.ImportPath path nil

