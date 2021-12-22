module IR.Spec
  ( module IR.Specs.Lexical,
    module IR.Specs.Type,
    module IR.Specs.Expression,
    module IR.Specs.Statement,
    module IR.Specs.Function,
    module IR.Specs.Contract,
    module IR.Specs.Program,
    IIdentifier', IType', IExpr', IStatement',
  )
where

import IR.Specs.Lexical
import IR.Specs.Expression
import IR.Specs.Type
import IR.Specs.Statement
import IR.Specs.Function
import IR.Specs.Contract
import IR.Specs.Program

type IIdentifier' = Maybe IIdentifier

type IType' = Maybe IType

type IExpr' = Maybe IExpr

type IStatement' = Maybe IStatement