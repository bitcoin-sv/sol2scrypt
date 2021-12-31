{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scrypt.Generables.Contract where

import Scrypt.Generables.Base
import Scrypt.Generables.Function ()
import Scrypt.Generables.Variable ()
import Scrypt.Spec as Scr
import Utils
import Data.List (intercalate)

instance Generable (Maybe (Scr.Contract Ann)) where
  genCode Nothing = return ""
  genCode (Just t) = genCode t

instance Generable (Scr.Contract Ann) where
  genCode (Scr.Contract cn _ props _ _ fs False _) = do
    cn' <- genCode cn
    let firstLine = "contract " ++ cn' ++ " {"
    incIndent
    props' <- mapM genCode props
    fs' <- mapM genCode fs
    decIndent
    let lastLine = "\n}"
    return $ firstLine ++ intercalate "\n" (props' ++ fs') ++ lastLine
  genCode _ = error "unimplemented `genCode` for contract"