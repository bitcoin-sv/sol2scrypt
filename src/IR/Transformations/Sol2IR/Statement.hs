{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Statement where

import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression 
import Solidity.Spec as Sol
import IR.Spec as IR
import IR.Transformations.Sol2IR.Type ()
import Protolude.Functor
import Data.Maybe (catMaybes)


instance ToIRTransformable VariableDeclaration IParam' where
  _toIR (Sol.VariableDeclaration a _ (Sol.Identifier pn)) = do
    t' <- _toIR a
    return $ IR.Param <$> t' <*> Just (IR.Identifier pn)


instance ToIRTransformable Sol.Statement IStatement' where
  _toIR (SimpleStatementExpression e) = ExprStmt <<$>> _toIR e
  _toIR (SimpleStatementVariableAssignmentList ids es) = do
    ids' <- mapM _toIR $ catMaybes ids
    es' <- mapM _toIR es
    let s = AssignStmt ids' $ catMaybes es'
    return $ Just s
  _toIR (SimpleStatementVariableDeclarationList declares es) = do
    declares' <- mapM _toIR $ catMaybes declares
    es' <- mapM _toIR es
    let s = DeclareStmt declares' $ catMaybes es'
    return $ Just s
  _toIR _ = return Nothing
