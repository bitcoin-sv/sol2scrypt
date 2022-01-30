{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE BlockArguments #-}
module IR.Transformations.Sol2IR.Statement where

import Data.Maybe
import IR.Transformations.Base hiding (returned)
import IR.Transformations.Sol2IR.Identifier
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Variable
import Solidity.Spec as Sol
import IR.Spec as IR
import IR.Transformations.Sol2IR.Type ()
import Protolude.Functor
import Utils

instance ToIRTransformable Sol.Statement IStatement' where
  _toIR (SimpleStatementExpression e) = do
    e' <- _toIR e
    withIfReturned $ Just $ ExprStmt $ fromJust e'
  _toIR (SimpleStatementVariableAssignmentList [Just i] [e]) = do
    e' <- _toIR e
    i' <- _toIR i
    i'' <- maybeStateVarId i'
    withIfReturned $ Just $ AssignStmt [i''] [fromJust e']
  _toIR (SimpleStatementVariableAssignmentList _ _) = error "unsupported SimpleStatementVariableAssignmentList"
  _toIR (SimpleStatementVariableDeclarationList [Just localVar] [e]) = do
    e' <- _toIR e
    localVar' <- _toIR localVar
    addSym $ Symbol <$> (paramName <$> localVar') <*> (paramType <$> localVar') <*> Just False
    withIfReturned $ Just $ DeclareStmt [localVar'] [fromJust e']
  _toIR (SimpleStatementVariableDeclarationList _ _) = error "unsupported SimpleStatementVariableDeclarationList"
  _toIR (Return e) = do
    setReturned
    e' <- case e of
            Just re -> _toIR re
            _ -> return $ Just $ IdentifierExpr (IR.Identifier "ret")
    let assignRet = IR.AssignStmt [Just $ IR.Identifier "ret"] [fromJust e']
    let assignReturned = IR.AssignStmt [Just $ IR.Identifier "returned"] [LiteralExpr $ BoolLiteral True]
    return $ Just $ IR.BlockStmt $ IR.Block [assignRet, assignReturned]
  _toIR (Sol.BlockStatement blk) = do
    blk' <- _toIR blk
    withIfReturned $ IR.BlockStmt <$> blk'
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
    return $ IR.Block <$> sequence (init stmts')


withIfReturned :: IStatement' -> Transformation IStatement'
withIfReturned stmt = do
  returned <- isReturned
  if returned then return $ wrapperStmt stmt else return stmt


wrapperStmt :: IStatement' -> IStatement'
wrapperStmt Nothing  = Nothing 
wrapperStmt (Just stmt) = Just $ IR.IfStmt (IR.UnaryExpr {unaryOp = Not, uExpr = IR.IdentifierExpr (IR.Identifier "returned")}) stmt Nothing 

