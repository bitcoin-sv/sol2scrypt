{-# LANGUAGE MultiParamTypeClasses #-}

module Intermediate.Transformations.Base where

import Control.Monad.State
import Solidity.Parser
import Text.Parsec hiding (try)

parseIO :: Parseable a => String -> IO a
parseIO solidityCode = either (fail . (parseError ++) . show) return $ parse parser "" solidityCode
  where
    parseError = "Error during parsing of <" ++ solidityCode ++ ">\n"

-----------------  Solidity to Intermediate -----------------

data TransformState = TransformState {}

type Transformation a = StateT TransformState IO a

class Parseable sol => ToIntermediateTransformable sol intmd where
  _toIntermediate :: sol -> Transformation intmd

transform2Intermediate :: ToIntermediateTransformable sol intmd => TransformState -> sol -> IO intmd
transform2Intermediate ts sol = fst <$> runStateT (_toIntermediate sol) ts

-----------------  Intermediate to sCrypt  -----------------

class ToScryptTransformable intmd scr where
  _toScrypt :: intmd -> scr

transform2Scrypt :: ToScryptTransformable intmd scr => intmd -> IO scr
transform2Scrypt = return . _toScrypt