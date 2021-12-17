{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Transformations.Base where

import Control.Monad.State
import Solidity.Parser
import Text.Parsec hiding (try)

parseIO :: Parseable a => String -> IO a
parseIO solidityCode = either (fail . (parseError ++) . show) return $ parse parser "" solidityCode
  where
    parseError = "Error during parsing of <" ++ solidityCode ++ ">\n"

-----------------  Solidity to IR -----------------

data TransformState = TransformState {}

type Transformation a = StateT TransformState IO a

class Parseable sol => ToIRTransformable sol ir where
  _toIR :: sol -> Transformation ir

transform2IR :: ToIRTransformable sol ir => TransformState -> sol -> IO ir
transform2IR ts sol = fst <$> runStateT (_toIR sol) ts

-----------------  IR to sCrypt  -----------------

class ToScryptTransformable ir scr where
  _toScrypt :: ir -> scr

transform2Scrypt :: ToScryptTransformable ir scr => ir -> IO scr
transform2Scrypt = return . _toScrypt