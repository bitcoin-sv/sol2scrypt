{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transpiler where

import Control.Monad.Except
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.IO as S
import IR
import Scrypt as Scr
import Solidity
import System.Directory
import Data.Maybe (catMaybes)
import Utils

-- Transpile Type Path: Solidity Type -> IR Type -> Scrypt Type 
-- Used in function `transpile` to provide types information
data TranspilePath sol ir scr = TranspilePath sol ir scr

data TranspileResult a b c = TranspileResult {scryptCode :: String, tranpilePath :: TranspilePath a b c}

transpile :: (Parseable a, ToIRTransformable a b, ToScryptTransformable b c, Generable c) => String -> IO (TranspileResult a b c)
transpile = transpile' (TransformState [] Nothing Map.empty [] Map.empty)

transpile' :: (Parseable a, ToIRTransformable a b, ToScryptTransformable b c, Generable c) => TransformState -> String -> IO (TranspileResult a b c)
transpile' initState solidityCode  = do
  sol :: a <- parseIO solidityCode
  itmd :: b <- transform2IR initState sol
  scr :: c <- transform2Scrypt itmd
  code <- generateScrypt (CodeGenState 0) scr
  -- the purpose of including `TranspilePath` is to provide the type values of `a`, `b`, `c` to Haskell compiler
  return $ TranspileResult code $ TranspilePath sol itmd scr


checkFilesExist :: [FilePath] -> IO ()
checkFilesExist deps = do
  mapM_ (\srcPath -> do
      fileExists <- liftIO $ doesFileExist srcPath
      if fileExists
        then return Nothing
        else error $ "File not found: \"" ++ srcPath ++ "\""
    ) deps


loadFile :: FilePath ->  IO String
loadFile srcPath = do
  if srcPath == "stdin"
    then liftIO . fmap T.unpack $ S.getContents
    else do
      checkFilesExist [srcPath]
      liftIO . fmap T.unpack . S.readFile $ srcPath


transpileFile :: FilePath -> IO String
transpileFile srcPath = do
  sol <- loadFile srcPath
  tr :: TranspileResult SolidityCode IProgram' (Maybe (Scr.Program Ann)) <- transpile sol
  return $ scryptCode tr

