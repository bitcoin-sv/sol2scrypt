{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Program where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

import Scrypt.Generables.Contract ()
import Utils
import Data.List (intercalate)


instance Generable (Maybe (Scr.ImportPath Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.ImportPath Ann) where
  genCode (Scr.ImportPath path _) = do
    return $ "import \"" ++ path ++ "\";"



instance Generable (Maybe (Scr.Program Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.Program Ann) where
  genCode (Scr.Program _imports _ _ contracts _) = do
    _imports' <- mapM genCode _imports
    contracts' <- mapM genCode contracts
    let importsLines = intercalate "\n" _imports'

    let contractsLines = intercalate "\n\n" contracts'

    return $ (if importsLines /= "" then importsLines ++ "\n\n" else "") ++ contractsLines