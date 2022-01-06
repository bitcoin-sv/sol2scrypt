{-# LANGUAGE FlexibleInstances #-}
module Scrypt.Generables.Base where

import Control.Monad.State
import Scrypt.Spec
import Utils
import Data.List (intercalate)

newtype CodeGenState = CodeGenState { unCodeGenState :: Int } deriving (Show, Eq, Ord)

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
  ind <- gets unCodeGenState
  modify $ \s -> s {unCodeGenState = ind + 1}

decIndent :: CodeGenerator ()
decIndent = do
  ind <- gets unCodeGenState
  modify $ \s -> s {unCodeGenState = if ind < 1 then error "negative indents" else ind - 1}

-- 2 spaces for a tab
tabWidth :: Int
tabWidth = 2

getIndent :: CodeGenerator String
getIndent = do
  ind <- gets unCodeGenState
  return $ intercalate "" $ replicate (tabWidth * ind) " "

withIndent :: String -> CodeGenerator String
withIndent s = do
  ind <- getIndent
  return $ "\n" ++ ind ++ trim s

withEmptyIndent :: CodeGenerator String
withEmptyIndent = do
  return ""

removeIndent :: String -> String
removeIndent (s:xs) = case s of
                      '\n' -> removeIndent xs
                      ' ' -> removeIndent xs
                      _ -> s:xs
removeIndent [] = ""

