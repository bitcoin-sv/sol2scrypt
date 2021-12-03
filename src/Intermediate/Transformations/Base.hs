module Intermediate.Transformations.Base where

import Control.Monad.State
import Solidity.Parser
import Text.Parsec hiding (try)

data TransformState = TransformState {}

type Transformation a = StateT TransformState IO a

parseIO :: Parseable a => String -> IO a
parseIO solidityCode = either (fail . (parseError ++) . show) return $ parse parser "" solidityCode
  where
    parseError = "Error during parsing of <" ++ solidityCode ++ ">\n"


