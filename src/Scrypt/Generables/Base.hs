{-# LANGUAGE FlexibleInstances #-}
module Scrypt.Generables.Base where

import Scrypt.Spec
import Utils

-- from sCrypt Ast to Code String
class Generable a where
  genCode :: a -> String

generateScrypt :: Generable a => a -> IO String
generateScrypt a = return $ genCode a

instance Generable (NameAnn a) where
  genCode (NameAnn n _) = n

instance Generable (Maybe (NameAnn Ann)) where
  genCode = maybe "" genCode