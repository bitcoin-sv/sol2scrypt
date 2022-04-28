{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Variable where

import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Type ()
import Solidity.Spec as Sol
import Solidity.Parser (display)

instance ToIRTransformable (Parameter SourceRange) IParam' where
  _toIR (Parameter t _ (Just i) _) = do
    t' <- _toIR t
    i' <- _toIR i
    return $ IR.Param <$> t' <*> i'
  _toIR p = reportError ("unsupported parameter: `" ++ display p ++ "`") (ann p) >> return Nothing


instance ToIRTransformable (VariableDeclaration SourceRange) IParam' where
  _toIR (Sol.VariableDeclaration t _ i _) = do
    t' <- _toIR t
    i' <- _toIR i
    return $ IR.Param <$> t' <*> i'


instance ToIRTransformable (Sol.StateVariableDeclaration SourceRange) IProperty' where
  _toIR (Sol.StateVariableDeclaration t vis i expr a) = do
    t' <- _toIR t
    vis' <- toIRVisibility vis
    i' <- _toIR i
    expr' <- _toIR expr
    let solConst = isSolConstant vis
    let solImmu = isSolImmutable vis

    let irStatic = isJust expr
        irConst = solConst || solImmu
        irState = not solConst && not solImmu

    case (irState, irStatic, irStatic == isJust expr') of
      (True, True, _) -> reportError "unsupported state variable with initial value" a >> return Nothing
      (_, _, False) -> return Nothing -- unsupported `expr`
      _ -> return $ IR.Property <$> i' <*> t' <*> Just vis' <*> Just expr' <*> Just (IsConst irConst) <*> Just (IsStatic irStatic) <*> Just (IsState irState)

toIRVisibility :: [String] -> Transformation IVisibility
toIRVisibility tags
  | "private" `elem` tags = return Private
  | "public" `elem` tags = return Public
  | otherwise = return Default

isSolConstant :: [String] -> Bool 
isSolConstant = elem "constant"

isSolImmutable :: [String] -> Bool 
isSolImmutable = elem "immutable"
