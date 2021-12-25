module IR.Spec
  ( module IR.Specs.Lexical,
    module IR.Specs.Type,
    module IR.Specs.Expression,
    module IR.Specs.Variable,
    module IR.Specs.Statement,
    module IR.Specs.Function,
    module IR.Specs.Contract,
    module IR.Specs.Empty,
    module IR.Specs.Program,
    IIdentifier', IType', IExpr', IStatement', IParam', IParamList',
    IVisibility', IBlock', IFunction',
    IContractBodyElement', IStateVariable',
  )
where

import IR.Specs.Lexical
import IR.Specs.Expression
import IR.Specs.Type
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

type IVisibility' = Maybe IVisibility

type IBlock' = Maybe IBlock

type IParam' = Maybe IParam

type IParamList' = Maybe IParamList

type IFunction' = Maybe IFunction

type IStateVariable' = Maybe IStateVariable

type IContractBodyElement' = Maybe IContractBodyElement
