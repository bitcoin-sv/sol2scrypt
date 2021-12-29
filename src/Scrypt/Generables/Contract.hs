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
  genCode = maybe "" genCode

instance Generable (Scr.Contract Ann) where
  genCode (Scr.Contract cn _ props _ _ fs False _) =
    "contract " ++ genCode cn ++ " { "
    ++ unwords (map genCode props)
    ++ " "
    ++ unwords (map genCode fs)
    ++ " }"
  genCode _ = error "unimplemented `genCode` for contract"