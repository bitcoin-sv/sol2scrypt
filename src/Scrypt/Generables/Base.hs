module Scrypt.Generables.Base where

-- from sCrypt Ast to Code String
class Generable a where
  genCode :: a -> String

generateScrypt :: Generable a => a -> IO String
generateScrypt a = return $ genCode a