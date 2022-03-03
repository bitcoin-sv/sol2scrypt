{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Variable where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Type ()
import Solidity.Spec as Sol

instance ToIRTransformable (Parameter SourceRange) IParam' where
  _toIR (Parameter t _ (Just i) _) = do
    t' <- _toIR t
    i' <- _toIR i
    return $ IR.Param <$> t' <*> i'
  _toIR p = reportError ("unsupported parameter `" ++ show p ++ "`") (ann p) >> return Nothing


instance ToIRTransformable (VariableDeclaration SourceRange) IParam' where
  _toIR (Sol.VariableDeclaration t _ i _) = do
    t' <- _toIR t
    i' <- _toIR i
    return $ IR.Param <$> t' <*> i'


instance ToIRTransformable (Sol.StateVariableDeclaration SourceRange) IStateVariable' where
  _toIR (Sol.StateVariableDeclaration t vis i expr a) = do
    t' <- _toIR t
    vis' <- toIRVisibility vis
    i' <- _toIR i
    expr' <- _toIR expr
    isConstant' <- isConstant vis
    isImmutable' <- isImmutable vis
    case (isConstant', expr') of 
      (False, Just _)  -> reportError "unsupported state variable with init value" a >> return Nothing
      (True , Nothing)  -> return Nothing
      _ -> return $ IR.StateVariable <$> i' <*> t' <*> Just vis' <*> Just expr' <*> Just isConstant' <*> Just isImmutable' 

toIRVisibility :: [String] -> Transformation IVisibility
toIRVisibility tags
  | "private" `elem` tags = return Private
  | "public" `elem` tags = return Public
  | otherwise = return Default

isConstant :: [String] -> Transformation Bool 
isConstant = return . elem "constant"

isImmutable :: [String] -> Transformation Bool 
isImmutable = return . elem "immutable"
