module IR.Spec
  ( module IR.Specs.Lexical,
    module IR.Specs.Type,
    module IR.Specs.Expression,
    module IR.Specs.Statement,
    module IR.Specs.Function,
    module IR.Specs.Contract,
    module IR.Specs.Empty,
    module IR.Specs.Program,
    module IR.Specs.Param,
    module IR.Specs.Variable,
    IIdentifier', IType', IExpr', IStatement', IParam', IParamList'
  )
where

import IR.Specs.Lexical
import IR.Specs.Expression
import IR.Specs.Type
import IR.Specs.Param
import IR.Specs.Statement
import IR.Specs.Function
import IR.Specs.Contract
import IR.Specs.Program
import IR.Specs.Empty
import IR.Specs.Variable

type IIdentifier' = Maybe IIdentifier

type IType' = Maybe IType

type IExpr' = Maybe IExpr

type IStatement' = Maybe IStatement

type IParam' = Maybe IParam 

type IParamList' = Maybe IParamList