{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Struct where

import Control.Monad.State
import IR.Spec as IR
import IR.Transformations.Base
import Solidity.Spec as Sol
import Data.Maybe
import IR.Transformations.Sol2IR.Variable ()

-- from TypeName to IType'
instance ToIRTransformable (StructDefinition SourceRange) IStruct' where
  _toIR (StructDefinition (Sol.Identifier n _) fields _) = do
    -- if a struct contains a mapping fields
    let source = foldl find Nothing fields
          where find acc x = case x of
                  (VariableDeclaration TypeNameMapping {} _ _ a ) -> if isJust acc then acc else Just a
                  _ -> Nothing
    case source of
      Nothing  -> do
        ss <- gets stateStructs
        fields' :: [IParam'] <- mapM _toIR fields
        modify $ \s ->
          s
            { stateStructs =
                IR.Struct n (catMaybes fields') : ss
            }
        return Nothing
      (Just a) -> reportError "unsupported struct with mapping field" a >> return Nothing