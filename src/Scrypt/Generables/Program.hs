{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Program where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

import Scrypt.Generables.Contract ()
import Utils
import Data.List (intercalate)

instance Generable (Maybe (Scr.Program Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.Program Ann) where
  genCode (Scr.Program _ _ _ contracts _) = do
    contracts' <- mapM genCode contracts
    return $ intercalate "\n" contracts'
