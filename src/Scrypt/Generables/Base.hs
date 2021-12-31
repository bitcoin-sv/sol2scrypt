{-# LANGUAGE FlexibleInstances #-}
module Scrypt.Generables.Base where

import Control.Monad.State
import Scrypt.Spec
import Utils
import Data.List (intercalate)

newtype CodeGenState = CodeGenState { stateIndentCount :: Int } deriving (Show, Eq, Ord)

type CodeGenerator a = StateT CodeGenState IO a

-- from sCrypt Ast to Code String
class Generable a where
  genCode :: a -> CodeGenerator String

generateScrypt :: Generable a => CodeGenState -> a -> IO String
generateScrypt cgs a = fst <$> runStateT (genCode a) cgs

instance Generable (NameAnn a) where
  genCode (NameAnn n _) = return n

instance Generable (Maybe (NameAnn Ann)) where
  genCode Nothing = return ""
  genCode (Just n)= genCode n

incIndent :: CodeGenerator ()
incIndent = do
  ind <- gets stateIndentCount
  modify $ \s -> s {stateIndentCount = ind + 1}

decIndent :: CodeGenerator ()
decIndent = do
  ind <- gets stateIndentCount
  modify $ \s -> s {stateIndentCount = if ind < 1 then 0 else ind - 1}

getIndent :: CodeGenerator String
getIndent = do
  ind <- gets stateIndentCount
  return $ intercalate "" $ replicate (2 * ind) " " {-- 2 space for a tab --}

withIndent :: String -> CodeGenerator String
withIndent s = do
  ind <- getIndent
  return $ "\n" ++ ind ++ trim s
