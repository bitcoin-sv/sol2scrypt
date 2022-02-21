{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Program where

import Scrypt.Generables.Base
import Scrypt.Spec as Scr

import Scrypt.Generables.Contract ()
import Scrypt.Generables.Struct ()
import Utils
import Data.List (intercalate)

instance Generable (Maybe (Scr.Program Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.Program Ann) where
  genCode (Scr.Program _imports _ structs contracts _) = do
    structs' <- mapM genCode structs
    _imports' <- mapM genCode _imports
    contracts' <- mapM genCode contracts
    let structsLines = intercalate "\n\n" structs'
    let importsLines = intercalate "\n" _imports'
    let contractsLines = intercalate "\n\n" contracts'
    return $ section importsLines ++ section structsLines ++ contractsLines
    where
      section = \code -> if code /= "" then code ++ "\n\n" else ""

instance Generable (Maybe (Scr.ImportPath Ann)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.ImportPath Ann) where
  genCode (Scr.ImportPath path _) = do
    return $ "import \"" ++ path ++ "\";"
