{-# LANGUAGE DeriveAnyClass #-}
-- use the DeriveGeneric extension and have the compiler generate the implementation for you
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- commit 01d3a3c392d8eaf8c061734cf837792d3714cc5b
module Scrypt.Spec where

import Data.Aeson hiding (Array, Bool)
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Word
import GHC.Generics hiding (Constructor)

type Name = String
data NameAnn a = NameAnn { unName :: String, annot :: a }
  deriving (Eq, Show, Generic, ToJSON, Functor, Foldable, Traversable)

instance Annotated NameAnn a where
  ann = annot

data TypeAnn a = TypeAnn { unType :: Type, annot :: a}
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
instance Annotated TypeAnn a where
  ann = annot

data TypeAnnWithGenerics a = TypeAnnWithGenerics { mType :: Type, genericTypes:: [TypeAnn a], annot :: a}
  deriving (Eq, Show, Functor, Foldable, Traversable)
instance Annotated TypeAnnWithGenerics a where
  ann = annot

-- newtype for type safety
-- is const
newtype Const = Const { unConst :: Bool } 
  deriving (Eq, Show)
-- is static
newtype Stc = Stc { unStatic :: Bool } 
  deriving (Eq, Show)
-- is contract used as instance (or type)
newtype ContractInstance = ContractInstance { usedAs :: Bool }
newtype IsStateProp = IsStateProp { unIsStateProp :: Bool } deriving (Eq, Show)

-- wildcard matching any array size
anyArraySize :: Integer
anyArraySize = -1

infixr 5 :->

-- compile time constant
data CTC = CTCConst Integer
         | CTCVar Name
         deriving (Eq)

instance Show CTC where
  show (CTCConst n) = if n == anyArraySize then "" else show n
  show (CTCVar n) = n

-- function param & return type: (type, isCtc)
data FuncParam = FuncParam { pCtc :: Bool, pType :: Type, pName:: Name}
  deriving (Show, Eq)

data Empty = Empty deriving (Show, Eq, Ord, Read)

data Type
  = Bool
  | Int
  | Bytes
  | SubBytes BytesType
  | PrivKey
  | ContractClass {contractType :: Name, isLib :: Bool, contractGenerics :: [Type]}
  | Array Type CTC
  | StructType Name
  | [FuncParam] :-> Type -- Function type
  | GenericType Name
  -- internal types that cannot be created by users
  | Void   -- only expr has a type, others do not
  | Any    -- wildcard type to by pass type checking
  | IfCond -- aggregate type to match multiple types allowed as if condition
  | StructLiteralType [Type]
  | CustomType Name
  deriving (Eq)

instance Show Type where
  show Bool = "bool"
  show Int = "int"
  show Bytes = "bytes"
  show PrivKey = "PrivKey"
  show (SubBytes t) = show t
  show (ContractClass n _ gts) = n ++ if null gts then "" else "<" ++ intercalate ", " (map show gts) ++ ">"
  show (Array t ctc) = showArr t [ctc]
      where
        -- int x[2][3] -> Array (Array int 3) 2 -> int[2][3]
        showArr et als = case et of
                          Array et' al' -> showArr et' $ als ++ [al']
                          _ -> show et ++ intercalate "" (map (\l -> "[" ++ show l ++ "]") als)
  show (StructType n) = "struct " ++ n ++ " {}"
  show (as :-> _) = "(" ++ intercalate ", " (map (\(FuncParam _ t _) -> show t) as) ++ ")"
  show (GenericType n) = n
  show Void = "void"
  show Any = "any"
  show IfCond = "if condition type"
  show (StructLiteralType ts) = "struct literal {" ++ intercalate ", " (map show ts) ++ "}"
  show (CustomType n) = n

data BytesType
  = PubKey
  | Sha1
  | Sha256
  | Ripemd160
  | Sig
  | SigHashType
  | OpCodeType
  | SigHashPreimage
  deriving (Eq)

instance Show BytesType where
  show PubKey = "PubKey"
  show Sha1 = "Sha1"
  show Sha256 = "Sha256"
  show Ripemd160 = "Ripemd160"
  show Sig = "Sig"
  show SigHashType = "SigHashType"
  show OpCodeType = "OpCodeType"
  show SigHashPreimage = "SigHashPreimage"

data Visibility = Public | Private | Default deriving (Eq, Show, Generic, Read, ToJSON)

data UnaryOp
  = Not
  | Invert
  | Negate
  | PreIncrement
  | PostIncrement
  | PreDecrement
  | PostDecrement
  deriving (Eq, Show, Read, Generic, ToJSON)

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor
  | BoolAnd
  | BoolOr
  | Equal
  | Neq
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | LShift
  | RShift
  | Dot
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | LShiftAssign
  | RShiftAssign
  | Index
  deriving (Eq, Show, Read, Generic, ToJSON)

class Annotated a b where
  -- extract annotation
  ann :: a b -> b

data Expr a
  = BoolLiteral {boolVal :: Bool, annot :: a}
  | IntLiteral {isHex :: Bool, intVal :: Integer, annot :: a}
  | BytesLiteral {isUTF8 :: Bool, bytesVal :: [Word8], annot :: a}
  | Var {variableName :: Name, isCTC :: Bool, annot :: a}
  | ArrayLiteral {arrayVal :: [Expr a], annot :: a}
  | StructLiteral {structVal :: [Expr a], annot :: a}
  | Slice {sliceArray :: Expr a, sliceStart :: Maybe (Expr a), sliceEnd :: Maybe (Expr a), annot :: a}
  | UnaryExpr {unaryOp :: UnaryOp, uExpr :: Expr a, annot :: a}
  | BinaryExpr {binaryOp :: BinaryOp, lExpr :: Expr a, rExpr :: Expr a, annot :: a}
  | TernaryExpr {ternaryCond :: Expr a, ternaryTrueBranch :: Expr a, ternaryFalseBranch :: Expr a, annot :: a}
  | Call {callFunc :: Name, callParams :: [Expr a], annot :: a}
  | Dispatch {dispatchContract :: Expr a {- contract class/object -}, dispatchContractGTs :: [TypeAnn a] {- contract generic types -}, dispatchMethod :: NameAnn a, dispatchParams :: [Expr a], annot :: a}
  | Parens {enclosedExpr :: Expr a, annot :: a}
  | NewExpr {newExprContract :: TypeAnnWithGenerics a, newExprParams :: [Expr a], annot :: a}
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Expr a where
  ann = annot

data Statement a
  = ExprStmt {stmtExpr :: Expr a, annot :: a}
  | Declare {declareParam :: Param a, declareExpr :: Expr a, annot :: a}
  | New {contractTypeL :: TypeAnnWithGenerics a, newObj :: NameAnn a, contractTypeR :: TypeAnnWithGenerics a, newParams :: [Expr a], annot :: a}
  | Assign {assignLhs :: Expr a, assignRhs :: Expr a, annot :: a}
  | Require {verifyExpr :: Expr a, annot :: a}
  | Exit {statusExpr :: Expr a, annot :: a}
  | If {ifCond :: Expr a, ifTrueBranch :: Statement a, ifFalseBranch :: Maybe (Statement a), annot :: a}
  | Block {blockStmts :: [Statement a], annot :: a}
  | Loop { loopCount :: Expr a, loopVar :: Maybe (NameAnn a), loopBody :: Statement a, annot :: a}
  | CodeSeparator {annot :: a}
  | ReturnStmt {retExpr :: Expr a, annot :: a}
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Statement a where
  ann = annot

data Asm a
  = Operator Name a
  | Operand [Word8] a
  | CtorVar Name a
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance Annotated Asm a where
  ann x = case x of
            Operator _ a -> a
            Operand _ a -> a
            CtorVar _ a -> a

data AsmBlock a = AsmBlock [Asm a] a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data FuncBody a
  = RegularBody [Statement a] (Expr a) a -- regular body with statements and return
  | AsmBody (AsmBlock a) a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Function a = Function
  { funcName :: NameAnn a,
    funcParams :: [Param a],
    funcReturnType :: TypeAnn a,
    funcBody :: FuncBody a,
    funcVisibility :: Visibility,
    funcStatic :: Stc,
    annot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CtorBody a = CtorBody [Statement a] a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Constructor a = Constructor
  { ctorParams :: [Param a],
    ctorBody :: CtorBody a,
    annot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Param a = Param
  { paramType :: TypeAnn a,
    paramName :: NameAnn a,
    paramConst :: Const,
    paramCtc :: Maybe CTC,
    paramVisibility :: Visibility,
    paramStateProp:: IsStateProp,
    annot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Param a where
  ann = annot

data Static a = Static
  { staticParam :: Param a,
    staticExpr :: Expr a,
    annot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Struct a = Struct
  { structName:: !Name,
    structFields :: ![Param a],
    annot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Annots are just compiler's metadata. After parsing, they are just references to source code positions.
-- After typechecking, they are expanded to contain also typechecking results (either error message or proper type).
-- note: Contract represent both regular contracts and libraries
data Contract a = Contract
  { contractName :: !(NameAnn a),
    contractGenericTypes :: [TypeAnn a],
    contractParams :: ![Param a],
    contractStatics :: ![Static a],
    contractConstructor :: !(Maybe (Constructor a)),
    contractFunctions :: ![Function a],
    -- is this a library
    contractLibrary :: !Bool,
    annot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ImportPath a = ImportPath FilePath a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TypeAlias a = TypeAlias (NameAnn a) (TypeAnn a) a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Program a = Program
  { programImports :: ![ImportPath a],
    programTypeAliases :: ![TypeAlias a],
    programStructs :: ![Struct a],
    programContracts :: ![Contract a],
    annot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- contains a program and all its dependencies' ASTs
type ProgramASTMap a = Map.Map FilePath (Program a)

type ProgramASTList a = [(FilePath, Program a)]