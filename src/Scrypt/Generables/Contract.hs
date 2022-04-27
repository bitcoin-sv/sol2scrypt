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
  genCode = maybe (return "") genCode

instance Generable (Scr.Contract Ann) where
  genCode (Scr.Contract cn _ props staticProps maybector fs False _) = do
    cn' <- genCode cn
    let firstLine = "contract " ++ cn' ++ " {"
    incIndent
    ctor' <- genCode maybector
    props' <- mapM (\p -> genCode (p, True)) props
    staticProps' <- mapM genCode staticProps
    fs' <- mapM genCode fs
    decIndent
    let lastLine = "\n}"
    return $ firstLine ++ intercalate "\n" (props' ++ staticProps' ++ ([ctor' | ctor' /= ""]) ++ fs') ++ lastLine
  genCode (Scr.Contract cn _ props staticProps maybector fs True _) = do
    cn' <- genCode cn
    let firstLine = "library " ++ cn' ++ " {"
    incIndent
    ctor' <- genCode maybector
    props' <- mapM genCode props
    staticProps' <- mapM genCode staticProps
    fs' <- mapM genCode fs
    decIndent
    let lastLine = "\n}"
    return $ firstLine ++ intercalate "\n" (props' ++ staticProps' ++ ([ctor' | ctor' /= ""]) ++ fs') ++ lastLine


instance Generable (Maybe (Scr.Constructor Ann)) where
  genCode Nothing = return ""
  genCode (Just t) = genCode t

instance Generable (Scr.Constructor Ann) where
  genCode (Scr.Constructor pl (Scr.CtorBody  stmts _) _) = do
    pl' <- mapM genCode pl
    firstLine  <- withIndent $ "constructor(" ++ intercalate ", " pl' ++ ") {"
    incIndent
    stmts' <- mapM genCode stmts
    decIndent
    lastLine <- withIndent "}"
    return $ firstLine ++ intercalate "" stmts' ++ lastLine