{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Statement where

import Control.Monad.State
import Data.Maybe
import qualified Data.Set as Set
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Identifier
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable ()
import Protolude.Functor
import Protolude.Monad (concatMapM)
import Solidity.Spec as Sol
import Utils
import IR.Transformations.Sol2IR.Helper

instance ToIRTransformable (Maybe (Sol.Statement SourceRange)) IStatement' where
  _toIR (Just s) = _toIR s
  _toIR _ = return Nothing


instance ToIRTransformable (Sol.Statement SourceRange) IStatement' where
  _toIR (SimpleStatementExpression (Binary (Operator "=" _) le re _) a) = do
    le' <- _toIR le
    checkLHSmapExpr le'
    re' <- _toIR re
    case le' of
      Just (BinaryExpr Index _ (IdentifierExpr _)) -> reportError "unsupported assign Statement, subscript cannot be a variable" a >> return Nothing
      _ -> return $ AssignStmt <$> sequence [le'] <*> sequence [re']
  _toIR (SimpleStatementExpression (FunctionCallExpressionList (Literal (PrimaryExpressionIdentifier (Sol.Identifier "require" _))) (Just (ExpressionList (e : _))) _) _) = do
    e' <- _toIR e
    return $ Just $ IR.RequireStmt $ fromJust e'
  _toIR (SimpleStatementExpression (FunctionCallExpressionList (Literal (PrimaryExpressionIdentifier (Sol.Identifier "assert" _))) (Just (ExpressionList (e : _))) _) _) = do
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
  _toIR (SimpleStatementVariableDeclarationList [Just localVar] [e] a) = do
    e' <- _toIR e
    localVar' <- _toIR localVar
    _ <- addSym $ Symbol <$> (paramName <$> localVar') <*> (paramType <$> localVar') <*> Just False
    case localVar' of 
      Nothing -> reportError "unsupported SimpleStatementVariableDeclarationList" a >> return Nothing
      Just _ -> return $ DeclareStmt [localVar'] <$>  sequence [e']
  _toIR (SimpleStatementVariableDeclarationList [Just localVar@(VariableDeclaration t _ n _)] [] a) = do
    localVar' <- _toIR localVar
    t' <- _toIR t
    e' <- defaultValueExpr t'
    case e' of 
      Nothing -> reportError ("unsupported declare `" ++ show t ++ "` without initializing it") a >> return Nothing
      _ -> return $ DeclareStmt [localVar'] <$>  sequence [e']
  _toIR (SimpleStatementVariableDeclarationList _ _ a) = reportError "unsupported SimpleStatementVariableDeclarationList" a >> return Nothing
  _toIR (Return e _) = do
    returned <- gets stateReturnedInBlock
    e' <- _toIR e
    let e'' = if isJust e' then e' else Just (LiteralExpr (BoolLiteral True))
    case returned of
      -- for the outermost return, transpile to `return returned ? retVal : e;`
      [True] -> do
        return $
          ReturnStmt
            <$> ( TernaryExpr
                    <$> Just (IdentifierExpr (IR.ReservedId varReturned))
                    <*> Just (IdentifierExpr (IR.ReservedId varRetVal))
                    <*> e''
                )
      -- for the non-outermost return (at least two flags in `stateReturnedInBlock`), transplie to `{ returned = true; retVal = e; }`
      _ : _ : _ -> do
        -- set current block's returned flag
        modify $ \s -> s {stateReturnedInBlock = True : drop 1 returned}
        return $
          BlockStmt
            <$> ( IR.Block
                    <$> sequence
                      [ AssignStmt <$> Just [IdentifierExpr (IR.ReservedId varRetVal)] <*> sequence [e''],
                        Just $ AssignStmt [IdentifierExpr (IR.ReservedId varReturned)] [LiteralExpr $ BoolLiteral True]
                      ]
                )
      _ -> return $ ReturnStmt <$> e''
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
  _toIR Sol.ForStatement {} = error "unexpected call `_toIR` for `ForStatement`, use the implemented in `instance ToIRTransformable (Sol.Statement SourceRange) [IStatement']`"
  _toIR Sol.WhileStatement {} = error "unexpected call `_toIR` for `WhileStatement`, use the implemented in `instance ToIRTransformable (Sol.Statement SourceRange) [IStatement']`"
  _toIR Sol.DoWhileStatement {} = error "unexpected call `_toIR` for `DoWhileStatement`, use the implemented in `instance ToIRTransformable (Sol.Statement SourceRange) [IStatement']`"
  _toIR (Sol.Break _) = do
    currentLoop <- getCurrentLoopId
    if isNothing currentLoop
      then return Nothing
      else do
        let breakFlag = IR.Identifier $ varLoopBreakFlag ++ show (fromJust currentLoop)
        return $ Just $ AssignStmt [IdentifierExpr breakFlag] [LiteralExpr $ BoolLiteral True]
  _toIR (Sol.Continue _) = do
    continuedLoops <- gets stateInFuncContinuedLoops
    currentLoop <- getCurrentLoopId
    if isNothing currentLoop
      then return Nothing
      else do
        -- mark the current loop has a `continue` statement
        modify $ \s -> s {stateInFuncContinuedLoops = Set.insert (fromJust currentLoop) continuedLoops}
        -- `IR.ContinueStmt` just exists during `Sol2IR` stage and must be transpile to an assignment statement, which is `loopContinueFlag<X> = true;`, before `IR2Scr` stage.
        return $ Just $ IR.ContinueStmt $ continueFlag $ fromJust currentLoop
  _toIR s = reportError ("unsupported statement `" ++ headWord (show s) ++ "`") (ann s) >> return Nothing

instance ToIRTransformable (Sol.Block SourceRange) IBlock' where
  _toIR (Sol.Block stmts _) = do
    enterScope
    stmts' <- transBlockStmtsWithReturn stmts []
    leaveScope
    return $ Just $ IR.Block $ catMaybes stmts'

instance ToIRTransformable (Sol.Statement SourceRange) [IStatement'] where
  _toIR (Sol.ForStatement (maybeInitStmt, maybeCheckExpr, maybeIterExpr) body _) = do
    lCount <- gets stateInFuncLoopCount
    loopIds <- gets stateCurrentLoopId
    modify $ \s -> s {stateInFuncLoopCount = lCount + 1, stateCurrentLoopId = lCount : loopIds}
    let currentLoop = lCount

    let breakFlag = IR.Identifier $ varLoopBreakFlag ++ show lCount
    let initBreakFlagStmt = IR.DeclareStmt [Just $ IR.Param (ElementaryType IR.Bool) breakFlag] [LiteralExpr $ BoolLiteral False]
    let hasBreakStmt = hasOwnLoopBreakStmt body
    initStmt <- _toIR maybeInitStmt
    -- only generate break-flag-init-stmt if got its own `break` stmt inside. NOTE: nested loop's break does not count.
    let initStmts = [Just initBreakFlagStmt | hasBreakStmt] ++ [initStmt | isJust initStmt]

    checkExpr <- _toIR maybeCheckExpr
    iterExpr <- _toIR maybeIterExpr
    let condExpr = case maybeCheckExpr of
          -- `checkExpr` or `!loopBreakFlag && checkExpr`
          Just _ -> if hasBreakStmt then BinaryExpr IR.BoolAnd <$> Just notBreakExpr <*> checkExpr else checkExpr
          -- !loopBreakFlag
          _ -> Just $ UnaryExpr IR.Not notBreakExpr
          where
            notBreakExpr = UnaryExpr IR.Not $ IdentifierExpr breakFlag

    enterScope
    bodyStmts <- case body of
      Sol.BlockStatement (Sol.Block ss _) -> concatMapM _toIR ss
      _ -> _toIR body
    continuedLoops <- gets stateInFuncContinuedLoops
    bodyStmts' <-
      if currentLoop `Set.member` continuedLoops
        then do
          let initContinueFlag = Just $ IR.DeclareStmt [Just $ IR.Param (IR.ElementaryType IR.Bool) $ continueFlag currentLoop] [IR.LiteralExpr $ IR.BoolLiteral False]
          modify $ \s -> s {stateContinuedInBlock = []}
          stmts <- transBlockStmtsWithContinue bodyStmts []
          return $ initContinueFlag : stmts
        else do
          return bodyStmts
    leaveScope

    let loopBodyStmt =
          IfStmt
            <$> condExpr
            <*> Just
              ( IR.BlockStmt
                  ( IR.Block $
                      catMaybes $ bodyStmts' ++ [ExprStmt <$> iterExpr]
                  )
              )
            <*> Just Nothing

    -- restore stacked loop ids
    modify $ \s -> s {stateCurrentLoopId = loopIds}

    return $
      initStmts
        ++ [ LoopStmt
               <$> Just (IdentifierExpr $ IR.Identifier $ varLoopCount ++ show lCount)
               <*> Just Nothing
               <*> loopBodyStmt
           ]
    where
      hasOwnLoopBreakStmt :: Sol.Statement SourceRange -> Bool
      hasOwnLoopBreakStmt (Sol.Break _) = True
      hasOwnLoopBreakStmt (Sol.BlockStatement (Sol.Block ss _)) = foldl (\r s -> r || hasOwnLoopBreakStmt s) False ss
      hasOwnLoopBreakStmt (Sol.IfStatement _ tBranch maybeFalseBranch _) = hasOwnLoopBreakStmt tBranch || maybe False hasOwnLoopBreakStmt maybeFalseBranch
      hasOwnLoopBreakStmt _ = False
  _toIR (Sol.WhileStatement expr stmt a) = _toIR $ Sol.ForStatement (Nothing, Just expr, Nothing) stmt a
  _toIR (Sol.DoWhileStatement stmt expr a) = do
    -- bcoz `stmt` may contain plain `break` statement (which should only work for the while loop), it must be removed before transpile the `stmt` to outer scope.
    stmt' <- concatMapM _toIR $ discardOwnLoopBreakStmt stmt
    enterScope
    loopStmts <- _toIR $ Sol.ForStatement (Nothing, Just expr, Nothing) stmt a
    leaveScope
    return $ stmt' ++ loopStmts
    where
      -- discard the statement's own `break` statement, Note: nested loop's `break` does not count.
      discardOwnLoopBreakStmt :: Statement SourceRange -> [Statement SourceRange]
      discardOwnLoopBreakStmt (Sol.Break _) = []
      discardOwnLoopBreakStmt (Sol.BlockStatement (Sol.Block ss ba)) = [Sol.BlockStatement (Sol.Block (concatMap discardOwnLoopBreakStmt ss) ba)]
      discardOwnLoopBreakStmt (Sol.IfStatement e tBranch maybeFalseBranch sa) =
        [Sol.IfStatement e tBranch' fBranch' sa]
        where
          tBranch' = case discardOwnLoopBreakStmt tBranch of
            x : _ -> x
            -- use empty block to replace the only `break` stmt in true branch.
            [] -> Sol.BlockStatement (Sol.Block [] (ann tBranch))
          fBranch' = case maybeFalseBranch of
            Just fb -> case discardOwnLoopBreakStmt fb of
              x : _ -> Just x
              [] -> Nothing
            _ -> Nothing
      discardOwnLoopBreakStmt s = [s]
  _toIR s = do
    s' <- _toIR s
    return [s']

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
        return (rss, results ++ stmt')
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

getCurrentLoopId :: Transformation (Maybe Integer)
getCurrentLoopId = do
  loops <- gets stateCurrentLoopId
  if null loops
    then return Nothing
    else return $ Just $ head loops

continueFlag :: Integer -> IIdentifier
continueFlag loopId = IR.Identifier $ varLoopContinueFlag ++ show loopId

transBlockStmtsWithContinue :: [IStatement'] -> [IStatement'] -> Transformation [IStatement']
transBlockStmtsWithContinue [] results = return results
transBlockStmtsWithContinue (stmt : rss) results = do
  continuedInBlock <- gets stateContinuedInBlock
  let previouslyContinued =
        if null continuedInBlock
          then Nothing
          else head continuedInBlock
  if isJust previouslyContinued
    then do
      let r = Just $
                 IR.IfStmt
                   (IR.UnaryExpr IR.Not (IR.IdentifierExpr $ fromJust previouslyContinued))
                   (IR.BlockStmt (IR.Block $ catMaybes $ stmt : rss))
                   Nothing
      r' <- transBlockStmtsWithContinue' r
      return $ results ++ [r']
    else do
      stmt' <- transBlockStmtsWithContinue' stmt
      transBlockStmtsWithContinue rss $ results ++ [stmt']

transBlockStmtsWithContinue' :: IStatement' -> Transformation IStatement'
transBlockStmtsWithContinue' (Just (IR.ContinueStmt cFlag)) = do
  continuedInBlock <- gets stateContinuedInBlock
  -- update current loop's continue flag
  modify $ \s -> s {stateContinuedInBlock = if null continuedInBlock then [Just cFlag] else Just cFlag : drop 1 continuedInBlock}
  return $ Just $ IR.AssignStmt [IR.IdentifierExpr cFlag] [IR.LiteralExpr $ IR.BoolLiteral True]
transBlockStmtsWithContinue' (Just (IR.IfStmt e tBranch maybeFalseBranch)) = do
  tBranch' <- do
    enterBlockWithContinue
    tStmts <- transBlockStmtsWithContinue (getBlockStmts tBranch) []
    leaveBlockWithContinue
    return $ IR.BlockStmt $ IR.Block $ catMaybes tStmts
  fBranch' <- case maybeFalseBranch of
    Just fBranch -> do
      enterBlockWithContinue
      fStmts <- transBlockStmtsWithContinue (getBlockStmts fBranch) []
      leaveBlockWithContinue
      return $ Just $ IR.BlockStmt $ IR.Block $ catMaybes fStmts
    _ -> return Nothing
  return $ Just $ IR.IfStmt e tBranch' fBranch'
  where
    getBlockStmts :: IStatement -> [IStatement']
    getBlockStmts (IR.BlockStmt (IR.Block ss)) = map Just ss
    getBlockStmts s = [Just $ IR.BlockStmt $ IR.Block [s]]
transBlockStmtsWithContinue' (Just (IR.BlockStmt (IR.Block ss))) = do
  enterBlockWithContinue
  ss' <- transBlockStmtsWithContinue (map Just ss) []
  let blockStmt = Just $ IR.BlockStmt $ IR.Block $ catMaybes ss'
  leaveBlockWithContinue
  return blockStmt
transBlockStmtsWithContinue' (Just (IR.LoopStmt e var body)) = do
  enterBlockWithContinue
  body' <- transBlockStmtsWithContinue' $ Just body
  leaveBlockWithContinue
  return $ IR.LoopStmt <$> Just e <*> Just var <*> body'
transBlockStmtsWithContinue' s = return s

enterBlockWithContinue :: Transformation ()
enterBlockWithContinue = do
  -- enter a new block, set `continueFlag` to Nothing
  continuedInBlock <- gets stateContinuedInBlock
  modify $ \s -> s {stateContinuedInBlock = Nothing : continuedInBlock}

leaveBlockWithContinue :: Transformation ()
leaveBlockWithContinue = do
  -- leave the block
  continuedInBlock' <- gets stateContinuedInBlock
  modify $ \s ->
    s
      { stateContinuedInBlock =
          case continuedInBlock' of
            -- keep outer scope's `continueFlag` if it exists
            _ : Just o : fs -> Just o : fs
            -- set outer scope's `continueFlag` with current if the outer does not exist
            flag : Nothing : fs -> flag : fs
            -- keep flags if has no outer scope
            flags -> flags
      }