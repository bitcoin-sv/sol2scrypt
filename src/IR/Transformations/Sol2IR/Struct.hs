{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Struct where

import Control.Monad.State
import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Variable ()
import Solidity.Spec as Sol

-- from TypeName to IType'
instance ToIRTransformable (StructDefinition SourceRange) IStruct' where
  _toIR (StructDefinition (Sol.Identifier n _) fields a) = do
    ss <- gets stateStructs
    if any (\st -> structName st == n) ss
      then reportError ("struct `" ++ n ++ "` already defined") a >> return Nothing
      else do
        -- if a struct contains a mapping fields
        let source = foldl find Nothing fields
              where
                find acc x = case x of
                  (VariableDeclaration TypeNameMapping {} _ _ a') -> if isJust acc then acc else Just a'
                  _ -> Nothing

        case source of
          Nothing -> do
            fields' :: [IParam'] <- mapM _toIR fields
            modify $ \s ->
              s
                { stateStructs = ss ++ [IR.Struct n (catMaybes fields')]
                }
            return Nothing
          (Just fa) -> reportError "unsupported struct definition with mapping field" fa >> return Nothing