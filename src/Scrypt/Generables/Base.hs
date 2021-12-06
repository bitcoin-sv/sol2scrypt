module Scrypt.Generables.Base where

-- from sCrypt Ast to Code String
class Generable a where
  genCode :: a -> String

generateScrypt :: Generable a => Maybe a -> IO String
generateScrypt (Just a) = return $ genCode a
generateScrypt Nothing = return ""