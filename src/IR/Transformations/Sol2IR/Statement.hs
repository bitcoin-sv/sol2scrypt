{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Statement where

import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Identifier
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable
import Protolude.Functor
import Solidity.Spec as Sol
import Utils

instance ToIRTransformable Sol.Statement IStatement' where
  _toIR (SimpleStatementExpression (Binary "=" le re)) = do
    le' <- _toIR le
    re' <- _toIR re
    return $ AssignStmt <$> sequence [le'] <*> sequence [re']
  _toIR (SimpleStatementExpression e) = ExprStmt <<$>> _toIR e
  _toIR (SimpleStatementVariableAssignmentList [Just i] [e]) = do
    e' <- _toIR e
    i' <- _toIR i
    i'' <- maybeStateVarId i'
    return $ AssignStmt <$> sequence [IdentifierExpr <$> i''] <*> sequence [e']
  _toIR (SimpleStatementVariableAssignmentList _ _) = error "unsupported SimpleStatementVariableAssignmentList"
  _toIR (SimpleStatementVariableDeclarationList [Just localVar] [e]) = do
    e' <- _toIR e
    localVar' <- _toIR localVar
    addSym $ Symbol <$> (paramName <$> localVar') <*> (paramType <$> localVar') <*> Just False
    return $ Just $ DeclareStmt [localVar'] [fromJust e']
  _toIR (SimpleStatementVariableDeclarationList _ _) = error "unsupported SimpleStatementVariableDeclarationList"
  _toIR (Return e) = do
    e' <- case e of
      Just re -> _toIR re
      _ -> return $ Just $ LiteralExpr (BoolLiteral True)
    return $ ReturnStmt <$> e'
  _toIR (Sol.BlockStatement blk) = do
    blk' <- _toIR blk
    return $ IR.BlockStmt <$> blk'
  _toIR Sol.EmitStatement {} = return Nothing
  _toIR (Sol.IfStatement e ifstmt maybeelsestmt) = do
    e' <- _toIR e
    ifstmt' <- _toIR ifstmt
    let ret = IR.IfStmt <$> e' <*> ifstmt'
    case maybeelsestmt of
      Just elsestmt -> do
        elsestmt' <- _toIR elsestmt
        return $ ret <*> (Just <$> elsestmt')
      Nothing -> return $ ret <*> Just Nothing
  _toIR s = error $ "unsupported statement `" ++ headWord (show s) ++ "`"

instance ToIRTransformable Sol.Block IBlock' where
  _toIR (Sol.Block stmts) = do
    enterScope
    stmts' <- mapM _toIR stmts
    leaveScope
    return $ IR.Block <$> sequence stmts'
