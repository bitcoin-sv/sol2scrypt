{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Statement where

import Control.Monad.State
import Data.Foldable (foldlM)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Helper
import IR.Transformations.Sol2IR.Identifier
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable ()
import Protolude.Functor
import Protolude.Monad (concatMapM)
import Solidity.Spec as Sol
import Solidity.Parser (display)

instance ToIRTransformable (Maybe (Sol.Statement SourceRange)) IStatement' where
  _toIR (Just s) = _toIR s
  _toIR _ = return Nothing

instance ToIRTransformable (Sol.Statement SourceRange) IStatement' where
  _toIR (SimpleStatementExpression (Binary (Operator "=" _) le re _) _) = do
    le' <- _toIR le
    checkLHSmapExpr le'
    re' <- _toIR re
    return $ AssignStmt <$> sequence [le'] <*> sequence [re']
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
  _toIR (SimpleStatementVariableAssignmentList vs es a) | length vs > 1 || length es > 1 = reportError "unsupported tuple expression assignment" a >> return Nothing
  _toIR s@(SimpleStatementVariableAssignmentList _ _ a) = reportError ("unsupported statement: `" ++ display s ++ "`") a >> return Nothing
  _toIR (SimpleStatementVariableDeclarationList [Just localVar] [e] _) = do
    e' <- _toIR e
    localVar' <- _toIR localVar
    _ <- addSym $ Symbol <$> (paramName <$> localVar') <*> (paramType <$> localVar') <*> Just False <*> Just False <*> Just False
    return $ DeclareStmt <$> sequence [localVar'] <*> sequence [e']
  _toIR (SimpleStatementVariableDeclarationList [Just localVar@(VariableDeclaration t _ _ _)] [] a) = do
    localVar' <- _toIR localVar
    t' <- _toIR t
    e' <- defaultValueExpr t'
    case e' of
      Nothing -> reportError "unsupported variable declaration without initializing it" a >> return Nothing
      _ -> return $ DeclareStmt <$> sequence [localVar'] <*> sequence [e']
  _toIR (SimpleStatementVariableDeclarationList vs es a) | length vs > 1 || length es > 1 = reportError "unsupported tuple expression declaration" a >> return Nothing
  _toIR (SimpleStatementVariableDeclarationList [Nothing] [] _) = return Nothing
  _toIR s@(SimpleStatementVariableDeclarationList _ _ a) = reportError ("unsupported statement: `" ++ display s ++ "`") a >> return Nothing
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
    brokeLoops <- gets stateInFuncBrokeLoops
    currentLoop <- getCurrentLoopId
    if isNothing currentLoop
      then return Nothing
      else do
        -- mark the current loop has a `break` statement
        modify $ \s -> s {stateInFuncBrokeLoops = Set.insert (fromJust currentLoop) brokeLoops}
        -- `IR.BreakStmt` just exists during `Sol2IR` stage and must be transpile to an assignment statement, which is `loopBreakFlag<X> = true;`, before `IR2Scr` stage.
        return $ Just $ IR.BreakStmt $ breakFlag $ fromJust currentLoop
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
  _toIR (Sol.Throw a) = reportError "unsupported deprecated throw statement" a >> return Nothing
  _toIR (Sol.InlineAssemblyStatement _ _ a) = reportError "unsupported assembly statement" a >> return Nothing
  _toIR e@Sol.SimpleStatementVariableList {} = reportError "unsupported deprecated var statement" (ann e) >> return Nothing

instance ToIRTransformable (Sol.Block SourceRange) IBlock' where
  _toIR (Sol.Block stmts _) = do
    enterScope
    stmts' <- transBlockStmtsWithReturn stmts []
    leaveScope
    return $ Just $ IR.Block $ catMaybes stmts'

instance ToIRTransformable (Sol.Statement SourceRange) [IStatement'] where
  _toIR (Sol.ForStatement (maybeInitStmt, maybeCheckExpr, maybeIterExpr) body _) = do
    loopId <- gets stateInFuncLoopCount
    loopIds <- gets stateNestedLoops
    modify $ \s -> s {stateInFuncLoopCount = loopId + 1, stateNestedLoops = loopId : loopIds}
    let currentLoop = loopId
        bFlag = breakFlag loopId
        notBreakExpr = UnaryExpr IR.Not $ IdentifierExpr bFlag

    initStmt <- _toIR maybeInitStmt
    checkExpr <- _toIR maybeCheckExpr
    iterExpr <- _toIR maybeIterExpr

    enterScope
    bodyStmts <- case body of
      Sol.BlockStatement blk -> do
        blk' :: IBlock' <- _toIR blk
        case blk' of
          Just (IR.Block ss) -> return $ map Just ss
          _ -> return []
      _ -> _toIR body

    brokeLoops <- gets stateInFuncBrokeLoops
    continuedLoops <- gets stateInFuncContinuedLoops
    let hasBreakStmt = currentLoop `Set.member` brokeLoops
        hasContinueStmt = currentLoop `Set.member` continuedLoops

    bodyStmts' <-
      if hasContinueStmt || hasBreakStmt
        then do
          let initContinueFlagStmt = Just $ IR.DeclareStmt [IR.Param (IR.ElementaryType IR.Bool) $ continueFlag currentLoop] [IR.LiteralExpr $ IR.BoolLiteral False]
          modify $ \s -> s {stateBCInBlock = []}
          stmts <- transBlockStmtsInLoop bodyStmts []
          return $ [initContinueFlagStmt | hasContinueStmt] ++ stmts
        else do
          return bodyStmts
    leaveScope

    let initBreakFlagStmt = IR.DeclareStmt [IR.Param (ElementaryType IR.Bool) bFlag] [LiteralExpr $ BoolLiteral False]
        -- only generate break-flag-init-stmt if got its own `break` stmt inside. NOTE: nested loop's break does not count.
        initStmts = [Just initBreakFlagStmt | hasBreakStmt] ++ [initStmt | isJust initStmt]

    let iterStmt =
          if hasBreakStmt
            then
              IfStmt
                <$> Just notBreakExpr
                <*> (IR.BlockStmt <$> (IR.Block <$> sequence [ExprStmt <$> iterExpr]))
                <*> Just Nothing
            else ExprStmt <$> iterExpr

    returned <- gets stateReturnedInBlock
    let condExpr = case maybeCheckExpr of
          Just _ ->
            if hasBreakStmt
              then -- `!loopBreakFlag && checkExpr`
                BinaryExpr IR.BoolAnd <$> Just notBreakExpr <*> checkExpr
              else -- `checkExpr`
                checkExpr
          _ ->
            if hasBreakStmt
              then Just notBreakExpr -- `!loopBreakFlag`
              else Just (LiteralExpr $ IR.BoolLiteral True)
        condExpr' =
          if not (null returned) && head returned
            then
              BinaryExpr
                <$> Just IR.BoolAnd <*> Just notReturnExpr <*> condExpr
            else condExpr
          where
            notReturnExpr = UnaryExpr IR.Not $ IdentifierExpr $ IR.ReservedId varReturned

    let loopBodyStmt =
          IfStmt
            <$> condExpr'
            <*> Just
              ( IR.BlockStmt
                  ( IR.Block $
                      catMaybes $ bodyStmts' ++ [iterStmt]
                  )
              )
            <*> Just Nothing

    -- restore stacked loop ids
    modify $ \s -> s {stateNestedLoops = loopIds}

    return $
      initStmts
        ++ [ LoopStmt
               <$> Just (IdentifierExpr $ IR.Identifier $ varLoopCount ++ show loopId)
               <*> Just Nothing
               <*> loopBodyStmt
           ]
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
    mc <- gets stateInFuncMappingCounter
    s' <- _toIR s
    mc' <- gets stateInFuncMappingCounter
    mapCheckStmts <-
      foldlM
        ( \ss (MECEntry t me ke _ _ i) -> do
            preCheckStmt' <- preCheckStmt (Just t) me ke "" i
            return $ ss ++ [Just preCheckStmt']
        )
        []
        $ Map.difference mc' mc
    return $ mapCheckStmts ++ [s']

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
  loops <- gets stateNestedLoops
  if null loops
    then return Nothing
    else return $ Just $ head loops

breakFlag :: Integer -> IIdentifier
breakFlag loopId = IR.Identifier $ varLoopBreakFlag ++ show loopId

continueFlag :: Integer -> IIdentifier
continueFlag loopId = IR.Identifier $ varLoopContinueFlag ++ show loopId

transBlockStmtsInLoop :: [IStatement'] -> [IStatement'] -> Transformation [IStatement']
transBlockStmtsInLoop [] results = return results
transBlockStmtsInLoop (stmt : rss) results = do
  bcInBlock <- gets stateBCInBlock
  let (bFlag, cFlag) = if null bcInBlock then (Nothing, Nothing) else head bcInBlock
  if isJust bFlag || isJust cFlag
    then do
      let condExpr = case (bFlag, cFlag) of
            (Just bf, Just cf) -> Just $ BinaryExpr IR.BoolAnd (notFlagExpr bf) (notFlagExpr cf)
            (Just bf, Nothing) -> Just $ notFlagExpr bf
            (Nothing, Just cf) -> Just $ notFlagExpr cf
            _ -> Nothing
      let r =
            IR.IfStmt <$> condExpr
              <*> Just (IR.BlockStmt (IR.Block $ catMaybes $ stmt : rss))
              <*> Just Nothing
      r' <- transBlockStmtsInLoop' r
      return $ results ++ [r']
    else do
      stmt' <- transBlockStmtsInLoop' stmt
      transBlockStmtsInLoop rss $ results ++ [stmt']
  where
    notFlagExpr flag = IR.UnaryExpr IR.Not $ IR.IdentifierExpr flag

transBlockStmtsInLoop' :: IStatement' -> Transformation IStatement'
transBlockStmtsInLoop' (Just (IR.BreakStmt bFlag)) = do
  bcInBlock <- gets stateBCInBlock
  let cFlag = if null bcInBlock then Nothing else snd $ head bcInBlock
  -- update current loop's break flag & keep continue flag
  modify $ \s -> s {stateBCInBlock = if null bcInBlock then [(Just bFlag, cFlag)] else (Just bFlag, cFlag) : drop 1 bcInBlock}
  return $ Just $ IR.AssignStmt [IR.IdentifierExpr bFlag] [IR.LiteralExpr $ IR.BoolLiteral True]
transBlockStmtsInLoop' (Just (IR.ContinueStmt cFlag)) = do
  bcInBlock <- gets stateBCInBlock
  let bFlag = if null bcInBlock then Nothing else fst $ head bcInBlock
  -- update current loop's continue flag & keep break flag
  modify $ \s -> s {stateBCInBlock = if null bcInBlock then [(bFlag, Just cFlag)] else (bFlag, Just cFlag) : drop 1 bcInBlock}
  return $ Just $ IR.AssignStmt [IR.IdentifierExpr cFlag] [IR.LiteralExpr $ IR.BoolLiteral True]
transBlockStmtsInLoop' (Just (IR.IfStmt e tBranch maybeFalseBranch)) = do
  tBranch' <- do
    enterBlockInLoop
    tStmts <- transBlockStmtsInLoop (getBlockStmts tBranch) []
    leaveBlockInLoop
    return $ IR.BlockStmt $ IR.Block $ catMaybes tStmts
  fBranch' <- case maybeFalseBranch of
    Just fBranch -> do
      enterBlockInLoop
      fStmts <- transBlockStmtsInLoop (getBlockStmts fBranch) []
      leaveBlockInLoop
      return $ Just $ IR.BlockStmt $ IR.Block $ catMaybes fStmts
    _ -> return Nothing
  return $ Just $ IR.IfStmt e tBranch' fBranch'
  where
    getBlockStmts :: IStatement -> [IStatement']
    getBlockStmts (IR.BlockStmt (IR.Block ss)) = map Just ss
    getBlockStmts s = [Just s]
transBlockStmtsInLoop' (Just (IR.BlockStmt (IR.Block ss))) = do
  enterBlockInLoop
  ss' <- transBlockStmtsInLoop (map Just ss) []
  leaveBlockInLoop
  return $ Just $ IR.BlockStmt $ IR.Block $ catMaybes ss'
transBlockStmtsInLoop' (Just (IR.LoopStmt e var body)) = do
  enterBlockInLoop
  body' <- transBlockStmtsInLoop' $ Just body
  leaveBlockInLoop
  return $ IR.LoopStmt <$> Just e <*> Just var <*> body'
transBlockStmtsInLoop' s = return s

enterBlockInLoop :: Transformation ()
enterBlockInLoop = do
  -- enter a new block, set `break` & `continue` flags to Nothing
  bcInBlock <- gets stateBCInBlock
  modify $ \s -> s {stateBCInBlock = (Nothing, Nothing) : bcInBlock}

leaveBlockInLoop :: Transformation ()
leaveBlockInLoop = do
  -- leave the block
  bcInBlock <- gets stateBCInBlock
  let breakFlags = map fst bcInBlock
      continueFlags = map snd bcInBlock
  modify $ \s ->
    s
      { stateBCInBlock =
          zip (propagateFlags breakFlags) (propagateFlags continueFlags)
      }
  where
    propagateFlags :: [IIdentifier'] -> [IIdentifier']
    -- keep outer block's flag if it exists
    propagateFlags (_ : Just o : fs) = Just o : fs
    -- set outer block's flag with current if the outer does not exist
    propagateFlags (flag : Nothing : fs) = flag : fs
    -- keep the flag if has no outer block
    propagateFlags flags = flags

-- -- require((!<mapExpr>.has({<keyExpr>, <idxExpr>})) && <valExpr> == <defaultValue> || <mapExpr>.canGet({<keyExpr>, <idxExpr>}, <valExpr>));
preCheckStmt :: IType' -> IExpression -> IExpression -> String -> Int -> Transformation IStatement
preCheckStmt t mapExpr keyExpr postfix idx = do
  defaultValue <- defaultValueExpr t
  return $
    IR.RequireStmt $
      BinaryExpr
        { binaryOp = BoolOr,
          lExpr =
            ParensExpr
              { enclosedExpr =
                  BinaryExpr
                    { binaryOp = BoolAnd,
                      lExpr =
                        UnaryExpr
                          { unaryOp = Not,
                            uExpr =
                              FunctionCallExpr
                                { funcExpr =
                                    MemberAccessExpr
                                      { instanceExpr = mapExpr,
                                        member = IR.Identifier "has"
                                      },
                                  funcParamExprs =
                                    [ StructLiteralExpr [keyExpr, fromJust $ indexExprOfMapping idx]
                                    ]
                                }
                          },
                      rExpr =
                        BinaryExpr
                          { binaryOp = IR.Equal,
                            lExpr = fromJust $ valueExprOfMapping e postfix,
                            rExpr = fromMaybe (LiteralExpr $ BoolLiteral False) defaultValue
                          }
                    }
              },
          rExpr = mapCanGetExpr mapExpr keyExpr postfix idx
        }
  where
    e = Just $ BinaryExpr Index mapExpr keyExpr

-- -- <mapExpr>.canGet({<keyExpr>, <idxExpr>}, <valExpr>)
mapCanGetExpr :: IExpression -> IExpression -> String -> Int -> IExpression
mapCanGetExpr mapExpr keyExpr postfix idx =
  let e = Just $ BinaryExpr Index mapExpr keyExpr
   in FunctionCallExpr
        { funcExpr =
            MemberAccessExpr
              { instanceExpr = mapExpr,
                member = IR.Identifier "canGet"
              },
          funcParamExprs =
            [ StructLiteralExpr [keyExpr, fromJust $ indexExprOfMapping idx],
              fromJust $ valueExprOfMapping e postfix
            ]
        }