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
import Data.Maybe (fromJust)


instance ToIRTransformable VariableDeclaration IParam' where
  _toIR (Sol.VariableDeclaration a _ (Sol.Identifier pn)) = do
    t' <- _toIR a
    return $ IR.Param <$> t' <*> Just (IR.Identifier pn)


instance ToIRTransformable Sol.Statement IStatement' where
  _toIR (SimpleStatementExpression e) = ExprStmt <<$>> _toIR e
  _toIR (SimpleStatementVariableAssignmentList [Just idtf] [e]) = do
    e' <- _toIR e
    idtf' <- _toIR idtf
    return $ Just $ AssignStmt [idtf'] [fromJust e']
  _toIR (SimpleStatementVariableDeclarationList [Just declare] [e]) = do
    e' <- _toIR e
    declare' <- _toIR declare
    return $ Just $ DeclareStmt [declare'] [fromJust e']
  _toIR (SimpleStatementVariableAssignmentList ids es) = error "unsupported SimpleStatementVariableAssignmentList"
  _toIR (SimpleStatementVariableDeclarationList declares es) = error "unsupported SimpleStatementVariableDeclarationList"
  _toIR _ = return Nothing
