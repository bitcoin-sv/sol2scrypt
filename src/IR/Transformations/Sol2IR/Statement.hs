{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Statement where

import Control.Monad.State
import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Identifier
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable ()
import Protolude.Functor
import Solidity.Spec as Sol
import Utils


instance ToIRTransformable (Sol.Statement SourceRange) IStatement' where
  _toIR (SimpleStatementExpression (Binary (Operator "=" _) le re _) a) = do
    le' <- _toIR le
    checkLHSmapExpr le'
    re' <- _toIR re
    case le' of
      Just (BinaryExpr Index _ (IdentifierExpr _)) -> reportError "unsupported assign Statement, subscript cannot be a variable" a >> return Nothing
      _ -> return $ AssignStmt <$> sequence [le'] <*> sequence [re']
  _toIR (SimpleStatementExpression (FunctionCallExpressionList (Literal (PrimaryExpressionIdentifier (Sol.Identifier "require" _))) (Just (ExpressionList (e:_))) _) _) = do
    e' <- _toIR e
    return $ Just $ IR.RequireStmt $ fromJust e'
  _toIR (SimpleStatementExpression (FunctionCallExpressionList (Literal (PrimaryExpressionIdentifier (Sol.Identifier "assert" _))) (Just (ExpressionList (e:_))) _) _) = do
    e' <- _toIR e
    return $ Just $ IR.RequireStmt $ fromJust e'
  _toIR (SimpleStatementExpression e _) = ExprStmt <<$>> _toIR e
  _toIR (SimpleStatementVariableAssignmentList [Just i] [e] _) = do
    e' <- _toIR e
    i' <- _toIR i
    i'' <- maybeStateVarId i'
    checkLHSmapExpr $ IdentifierExpr <$> i''
    return $ AssignStmt <$> sequence [IdentifierExpr <$> i''] <*> sequence [e']
  _toIR (SimpleStatementVariableAssignmentList _ _ a) = reportError "unsupported SimpleStatementVariableAssignmentList" a >> return Nothing
  _toIR (SimpleStatementVariableDeclarationList [Just localVar] [e] _) = do
    e' <- _toIR e
    localVar' <- _toIR localVar
    addSym $ Symbol <$> (paramName <$> localVar') <*> (paramType <$> localVar') <*> Just False
    return $ DeclareStmt [localVar'] <$>  sequence [e']
  _toIR (SimpleStatementVariableDeclarationList _ _ a) = reportError "unsupported SimpleStatementVariableDeclarationList" a >> return Nothing
  _toIR (Return e _) = do
    returned <- gets stateReturnedInBlock
    modify $ \s -> s {stateReturnedInBlock = True : drop 1 returned}
    e' <- _toIR e
    let e'' = (if isJust e' then e' else Just (LiteralExpr (BoolLiteral True)))
    return $
      BlockStmt
        <$> ( IR.Block
                <$> sequence
                  [ AssignStmt <$> Just [IdentifierExpr (IR.ReservedId varRetVal)] <*> sequence [e''],
                    Just $ AssignStmt [IdentifierExpr (IR.ReservedId varReturned)] [LiteralExpr $ BoolLiteral True]
                  ]
            )

  _toIR (Sol.BlockStatement blk) = do
    blk' <- _toIR blk
    return $ IR.BlockStmt <$> blk'
  _toIR Sol.EmitStatement {} = return Nothing
  _toIR Sol.PlaceholderStatement {} = return Nothing
  _toIR Sol.RevertStatement {} = return $ Just $ IR.RequireStmt $ LiteralExpr $ BoolLiteral False
  _toIR (Sol.IfStatement e ifstmt maybeelsestmt _) = do
    -- wrap a single return statement into a block for the convenience of transpile `return`
    let wrapSingleRet stmt = case stmt of
          r@(Return _ a) -> Sol.BlockStatement $ Sol.Block [r] a
          _ -> stmt
    e' <- _toIR e
    ifstmt' <- _toIR $ wrapSingleRet ifstmt
    let ret = IR.IfStmt <$> e' <*> ifstmt'
    case maybeelsestmt of
      Just elsestmt -> do
        elsestmt' <- _toIR $ wrapSingleRet elsestmt
        return $ ret <*> (Just <$> elsestmt')
      Nothing -> return $ ret <*> Just Nothing
  _toIR s = reportError ("unsupported statement `" ++ headWord (show s) ++ "`") (ann s) >> return Nothing

instance ToIRTransformable (Sol.Block SourceRange) IBlock' where
  _toIR (Sol.Block stmts _) = do
    enterScope
    stmts' <- transBlockStmtsWithReturn stmts []
    leaveScope
    return $ Just $ IR.Block $ catMaybes stmts'

-- transplie block statments that may have returned in middle
transBlockStmtsWithReturn :: [Statement SourceRange] -> [IStatement'] -> Transformation [IStatement']
transBlockStmtsWithReturn [] results = return results
transBlockStmtsWithReturn ss@(stmt : rss) results = do
  returned <- gets stateReturnedInBlock
  (ss', results') <-
    case returned of
      -- when it's the outermost block
      [True] -> do
        case last ss of
          -- end with return stmt
          r@Return {} -> do
            -- wrap all stmts except the last return into a if stmt: `if (!returned) {...}`
            ifstmt <- wrapWithIfReturn $ init ss
            r' <- _toIR r
            return ([], results ++ ifstmt ++ [r'])
          -- end with non return stmt
          _ -> do
            -- wrap all stmts after into a if stmt: `if (!returned) {...}`
            ifstmt <- wrapWithIfReturn ss
            return ([], results ++ ifstmt)
      -- when it's not the outermost block
      True : _ -> do
        -- wrap all stmts after into a if stmt: `if (!returned) {...}`
        ifstmt <- wrapWithIfReturn ss
        return ([], results ++ ifstmt)
      _ -> do
        stmt' <- _toIR stmt
        return (rss, results ++ [stmt'])
  transBlockStmtsWithReturn ss' results'
  where
    -- wrap `stmts` to `if (!returned) { <stmts> }`
    wrapWithIfReturn stmts = do
      if null stmts
        then return []
        else do
          blk <- _toIR $ Sol.BlockStatement $ Sol.Block stmts $ mergeRange (ann $ head stmts) (ann $ last stmts)
          case blk of
            Just blk' ->
              return
                [ Just $
                    IR.IfStmt
                      (UnaryExpr Not $ IdentifierExpr $ IR.ReservedId varReturned)
                      blk'
                      Nothing
                ]
            _ -> return []
