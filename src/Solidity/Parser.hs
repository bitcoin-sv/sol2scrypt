-- Copyright 2017 Gordon J. Pace and Joshua Ellul
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- Bug in Sublime Text syntax highlighting for Haskel ("()"...

module Solidity.Parser where

import Control.Monad
import Data.Char hiding (DecimalNumber)
import Data.Functor
import Data.List
import Solidity.Spec
import Text.Parsec
import Text.Parsec.String

class Parseable a where
  display :: a -> String
  parser :: Parser a

instance Parseable a => Parseable (Maybe a) where
  parser = Just <$> try parser <|> return Nothing
  display Nothing = ""
  display (Just x) = display x

_display :: Parseable a => a -> String
_display = (' ' :) . display

lineDisplay :: Parseable a => a -> String
lineDisplay = filter (not . isSpace) . display

-- Some helper parsing functions
indent :: String -> String
indent = unlines . map ("  " ++) . lines

sep1, sep :: Char -> Parser a -> Parser [a]
sep1 c p = try ((:) <$> p <*> (whitespace *> char c *> whitespace *> sep1 c p)) <|> return <$> p
sep c p = try (sep1 c p) <|> return []

semicolonSep, semicolonSep1, commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep = sep ','
commaSep1 = sep1 ','
semicolonSep = sep ';'
semicolonSep1 = sep1 ';'

pair :: a -> b -> (a, b)
pair x y = (x, y)

comment :: Parser ()
comment =
  try ((string "//" *> manyTill anyChar (eof <|> newline $> ())) Data.Functor.$> ())
    <|> string "/*" *> manyTill anyChar (try $ string "*/") $> ()

whitespace, whitespace1 :: Parser ()
whitespace = () <$ many (try comment <|> () <$ space)
whitespace1 = (comment <|> spaces) >> whitespace

endOfWord :: Parser ()
endOfWord = eof <|> notFollowedBy (alphaNum <|> oneOf "$_")

list :: [Parser a] -> Parser [a]
list [] = return []
list (p : ps) = do vp <- p; vps <- list ps; return (vp : vps)

keyword :: String -> Parser String
keyword word = string word <* endOfWord

addSource :: Parser (SourceRange -> a) -> Parser a
addSource p = do
  start <- getPosition
  x <- p
  x . SourceRange start <$> getPosition

-- end of helper functions

-------------------------------------------------------------------------------

instance Parseable (SolidityCode SourceRange) where
  display (SolidityCode c) = display c
  parser = whitespace *> (SolidityCode <$> parser) <* whitespace <* eof

-------------------------------------------------------------------------------
-- SourceUnit = (PragmaDirective | ImportDirective | ContractDefinition | ErrorDefinition)*

instance Parseable (SourceUnit SourceRange) where
  parser =
    SourceUnit
      <$> many (parser <* whitespace)
  display (SourceUnit us) = unlines $ map display us

instance Parseable (SourceUnit1 SourceRange) where
  parser =
    try (SourceUnit1_PragmaDirective <$> parser) <|>
    try (SourceUnit1_ImportDirective <$> parser) <|>
    try (SourceUnit1_ErrorDefinition <$> parser) <|>
    try (SourceUnit1_StructDefinition <$> parser) <|>
    (SourceUnit1_ContractDefinition <$> parser)
  display (SourceUnit1_ContractDefinition contract_definition) = display contract_definition
  display (SourceUnit1_ErrorDefinition error_definition) = display error_definition
  display (SourceUnit1_ImportDirective import_directive) = display import_directive
  display (SourceUnit1_PragmaDirective pragma_directive) = display pragma_directive
  display (SourceUnit1_StructDefinition s) = display s
-------------------------------------------------------------------------------
-- data VersionComparator = Less | More | Equal | LessOrEqual | MoreOrEqual deriving (Show, Eq, Ord)

instance Parseable VersionComparator where
  parser =
    try $
      choice
        [ do
            string ">"
            try
              ( do
                  string "="
                  return MoreOrEqual
              )
              <|> return More,
          do
            string "<"
            try
              ( do
                  string "="
                  return LessOrEqual
              )
              <|> return Less,
          do
            char '^'
            return Equal
        ]
        <|> return Equal
  display More = ">"
  display Less = "<"
  display MoreOrEqual = ">="
  display LessOrEqual = "<="
  display Equal = "^"

--  data Version = Version VersionComparator [Int] deriving (Show, Eq, Ord)

instance Parseable Version where
  parser = do
    c <- parser
    version <- many digit `sepBy` char '.'
    return $ Version c (map (read :: [Char] -> Int) version)

  display (Version c version) = display c ++ intercalate "." [show n | n <- version]

instance Parseable (PragmaDirective SourceRange) where
  parser =
    string "pragma" *> whitespace1
      *> choice
        [ do
            start <- getPosition
            string "experimental"
            whitespace1
            t <- manyTill anyChar (char ';')
            ExperimentalPragma t . SourceRange start <$> getPosition,
          do
            start <- getPosition
            string "solidity"
            whitespace1
            try
              ( do
                  vs <- (whitespace1 *> parser <* whitespace1) `sepBy` string "||"
                  whitespace1 <* char ';'
                  SolidityPragmaDisjunction vs . SourceRange start <$> getPosition
              )
              <|> ( do
                      vs <- parser `sepBy` (char ' ' *> whitespace)
                      whitespace1 *> char ';'
                      SolidityPragmaConjunction vs . SourceRange start <$> getPosition
                  )
        ]

  display (SolidityPragmaConjunction versions _) = "pragma solidity " ++ unwords (map display versions) ++ ";"
  display (SolidityPragmaDisjunction versions _) = "pragma solidity " ++ intercalate "||" (map display versions) ++ ";"
  display (ExperimentalPragma l _) = "pragma experimental " ++ l ++ ";"

-------------------------------------------------------------------------------
-- ImportDirective = 'import' StringLiteral ('as' Identifier)? ';'
--         | 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
--         | 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'

instance Parseable (ImportDirective SourceRange) where
  parser = keyword "import" *> whitespace1 *> choice [directive1, directive2, directive3] <* whitespace <* char ';'
    where
      parseIdentifierOrStar :: Parser (Import SourceRange)
      parseIdentifierOrStar = ImportId <$> parser

      parseMaybeAsIdentifier :: Parser (Maybe (Identifier SourceRange))
      parseMaybeAsIdentifier =
        Just <$> (keyword "as" *> whitespace1 *> parser) <|> return Nothing

      directive1 =
        do
          start <- getPosition
          _from <- parser <* whitespace
          _as <- parseMaybeAsIdentifier
          end <- getPosition
          let pos = SourceRange start end
          return $ ImportDirective [ImportDirective1 (ImportAll pos) _as pos] _from pos

      directive2 =
        do
          pos0 <- getPosition
          _name <- parseIdentifierOrStar <* whitespace1
          _as <- parseMaybeAsIdentifier <* whitespace
          pos1 <- getPosition
          _from <- keyword "from" *> whitespace1 *> parser
          ImportDirective [ImportDirective1 _name _as $ SourceRange pos0 pos1] _from . SourceRange pos0 <$> getPosition

      directive3 =
        do
          start <- getPosition
          char '{' *> whitespace
          _directives <-
            commaSep1
              ( do
                  start_ <- getPosition
                  _name <- parser <* whitespace1
                  _as <- parseMaybeAsIdentifier <* whitespace
                  ImportDirective1 (ImportId _name) _as . SourceRange start_ <$> getPosition
              )
          char '}' *> whitespace
          _from <- keyword "from" *> whitespace1 *> parser
          ImportDirective _directives _from . SourceRange start <$> getPosition

  display directive =
    ( case map displayImport (imports directive) of
        [i] -> i
        is -> "{ " ++ intercalate ", " is ++ " }"
    )
      ++ " from "
      ++ display (from directive)
    where
      displayImport i =
        case name i of
          ImportAll _ -> "*" ++ textAs
          ImportId identifier -> display identifier ++ textAs
        where
          textAs =
            case as i of
              Nothing -> ""
              Just identifier' -> " as " ++ display identifier'


-------------------------------------------------------------------------------
-- ErrorDefinition = 'error' Identifier ParameterList ';'

instance Parseable (ErrorDefinition SourceRange)  where
  parser =
    do
      start <- getPosition
      i <- keyword "error" *> whitespace *> parser <* whitespace
      pl <- parser <* whitespace <* char ';'
      ErrorDefinition i pl . SourceRange start <$> getPosition
  display err = "error " ++ _display (errorName err) ++ _display (parameters err) ++ ";"


-------------------------------------------------------------------------------
-- StructDefinition = 'struct' Identifier ParameterList ';'

instance Parseable (StructDefinition SourceRange)  where
  parser =
    do
      start <- getPosition
      i <- keyword "struct" *> whitespace *> parser <* whitespace <* char '{' <* whitespace
      vs <- many (parser <* whitespace <* char ';' <* whitespace) <* char '}'
      StructDefinition i vs . SourceRange start <$> getPosition
  display (StructDefinition i vs _) = "struct " ++ display i ++ " {\n" ++ indent (intercalate ";\n" (map display vs) ++ ";") ++ "}"



-------------------------------------------------------------------------------
-- ContractDefinition = ( 'contract' | 'library' | 'interface' ) Identifier
--                      ( 'is' InheritanceSpecifier (',' InheritanceSpecifier )* )?
--                      '{' ContractPart* '}'

instance Parseable (ContractDefinition SourceRange) where
  parser = do
    start <- getPosition
    _abstract <- (try (keyword "abstract" $> True) <|> return False) <* whitespace
    _definitionType' <- (keyword "contract" <|> keyword "library" <|> keyword "interface") <* whitespace
    _definitionName' <- parser <* whitespace
    _isClause' <- (try (keyword "is" *> whitespace *> commaSep1 parser) <|> return []) <* whitespace
    _contractParts' <- char '{' *> whitespace *> many (parser <* whitespace) <* char '}'
    ContractDefinition _abstract _definitionType' _definitionName' _isClause' _contractParts' . SourceRange start <$> getPosition

  display contractDefinition =
    definitionType contractDefinition ++ _display (definitionName contractDefinition)
      ++ (if null isClauses then "" else " is " ++ intercalate ", " isClauses)
      ++ " {\n"
      ++ indent (concatMap (\p -> display p ++ "\n") (contractParts contractDefinition))
      ++ "\n}"
    where
      isClauses = map display $ isClause contractDefinition

-------------------------------------------------------------------------------
-- ContractPart
--    = 'using' Identifier 'for' ('*' | TypeName) ';'
--    | 'struct' Identifier '{' ( VariableDeclaration ';' (VariableDeclaration ';')* )? '}'
--    | 'modifier' Identifier ParameterList? Block
--    | 'function' Identifier? ParameterList  ( FunctionDefinitionTag )* ( 'returns' ParameterList )? ( ';' | Block )
--    | 'enum' Identifier '{' EnumValue? (',' EnumValue)* '}'
--    | 'event' Identifier IndexedParameterList 'anonymous'? ';'
--    | StateVariableDeclaration

instance Parseable (ContractPart SourceRange) where
  parser =
    try
      ( choice
          [ do
              start <- getPosition
              i <- keyword "using" *> whitespace *> parser <* whitespace <* keyword "for" <* whitespace
              tn <- (Nothing <$ char '*' <|> Just <$> parser) <* whitespace <* char ';'
              ContractPartUsingForDeclaration i tn . SourceRange start <$> getPosition,
            do
              start <- getPosition
              i <- keyword "modifier" *> whitespace *> parser <* whitespace
              pl <- (Just <$> parser <|> return Nothing) <* whitespace
              b <- parser
              ContractPartModifierDefinition i pl b . SourceRange start <$> getPosition,
            do
              start <- getPosition
              mi <- keyword "function" *> whitespace *> parser <* whitespace
              ps <- parser <* whitespace
              ts <- many (parser <* whitespace)
              mps' <- (try (Just <$> keyword "returns" *> whitespace *> parser) <|> return Nothing) <* whitespace
              b <- Nothing <$ char ';' <|> Just <$> parser
              ContractPartFunctionDefinition mi ps ts mps' b . SourceRange start <$> getPosition,
            do
              start <- getPosition
              ps <- keyword "constructor" *> whitespace *> parser <* whitespace
              ts <- many (parser <* whitespace)
              b <- Nothing <$ char ';' <|> Just <$> parser
              ContractPartConstructorDefinition ps ts b . SourceRange start <$> getPosition,
            char 'e'
              *> ( do
                     start <- getPosition
                     i <- keyword "num" *> whitespace *> parser <* whitespace <* char '{' <* whitespace
                     vs <- commaSep parser <* whitespace <* char '}'
                     ContractPartEnumDefinition i vs . SourceRange start <$> getPosition
                     <|> do
                       start <- getPosition
                       i <- keyword "vent" *> whitespace *> parser <* whitespace
                       ipl <- parser <* whitespace
                       a <- (True <$ keyword "anonymous" <|> return False) <* whitespace <* char ';'
                       ContractPartEventDefinition i ipl a . SourceRange start <$> getPosition
                 )
          ]
      )
      <|> try ( do
              start <- getPosition
              d <- parser
              ContractPartStateVariableDeclaration d . SourceRange start <$> getPosition
          )
      <|> try ( do
              ContractPartStructDefinition <$> parser
          )
  display (ContractPartUsingForDeclaration v t _) =
    "using " ++ display v ++ " for " ++ maybe "*" display t ++ ";"
  display (ContractPartEnumDefinition i vs _) =
    "enum " ++ display i ++ " {" ++ intercalate ", " (map display vs) ++ "}"
  display (ContractPartStructDefinition st) = display st
  display (ContractPartModifierDefinition i pl b _) =
    "modifier " ++ display i ++ maybe "" _display pl ++ _display b
  display (ContractPartFunctionDefinition mi pl ts mpl' mb _) =
    "function" ++ maybe "" _display mi ++ _display pl
      ++ concatMap _display ts
      ++ maybe "" ((" returns " ++) . display) mpl'
      ++ maybe ";" _display mb
  display (ContractPartConstructorDefinition pl ts mb _) =
    "constructor" ++ _display pl
      ++ concatMap _display ts
      ++ maybe ";" _display mb
  display (ContractPartEventDefinition i ipl a _) =
    "event " ++ display i ++ _display ipl ++ (if a then " anonymous" else "") ++ ";"
  display (ContractPartStateVariableDeclaration v _) = display v

-------------------------------------------------------------------------------
-- StateVariableDeclaration = TypeName ( 'public' | 'internal' | 'private' | 'constant' | 'immutable' )? Identifier ('=' Expression)? ';'

instance Parseable (StateVariableDeclaration SourceRange) where
  parser =
    do
      start <- getPosition
      _typename' <- parser <* whitespace
      _visibility' <-
        many
          ( choice
              [ try $ keyword "public",
                try $ keyword "private",
                try $ keyword "internal",
                try $ keyword "constant",
                try $ keyword "immutable"
              ]
              <* whitespace
          )
      _variableName' <- parser <* whitespace
      _initialValue' <- try (Just <$> (char '=' *> whitespace *> parser)) <|> return Nothing
      _ <- whitespace *> char ';'
      StateVariableDeclaration _typename' _visibility' _variableName' _initialValue' . SourceRange start <$> getPosition

  display v =
    display (typename v)
      ++ (if null (visibility v) then "" else " " ++ unwords (visibility v))
      ++ _display (variableName v)
      ++ maybe "" (\i -> " = " ++ display i) (initialValue v)
      ++ ";"

-------------------------------------------------------------------------------
-- InheritanceSpecifier = UserDefinedTypeName ( '(' Expression ( ',' Expression )* ')' )?

instance Parseable (InheritanceSpecifier SourceRange) where
  parser =
    do
      start <- getPosition
      _userDefinedTypeName' <- parser <* whitespace
      _inheritanceParameters' <- try (char '(' *> whitespace *> commaSep1 parser <* whitespace <* char ')') <|> return []
      InheritanceSpecifier _userDefinedTypeName' _inheritanceParameters' . SourceRange start <$> getPosition

  display spec =
    display (userDefinedTypeName spec)
      ++ (if null (inheritanceParameters spec) then "" else "(" ++ intercalate ", " (map display $ inheritanceParameters spec) ++ ")")

-------------------------------------------------------------------------------
-- ModifierInvocation = Identifier ( '(' ExpressionList? ')' )?

instance Parseable (ModifierInvocation SourceRange) where
  parser =
    do
      start <- getPosition
      i <- parser <* whitespace
      unless (display i /= "returns") mzero
      es <- try (char '(' *> whitespace *> parser <* whitespace <* char ')') <|> return Nothing
      ModifierInvocation i es . SourceRange start <$> getPosition
  display mi =
    display (modifierInvocationIdentifier mi)
      ++ maybe "" (\s -> "(" ++ display s ++ ")") (modifierInvocationParameters mi)

-------------------------------------------------------------------------------
-- FunctionDefinitionTag = ModifierInvocation | StateMutability | 'public' | 'internal' | 'private'

instance Parseable (FunctionDefinitionTag SourceRange) where
  display (FunctionDefinitionTagModifierInvocation m) = display m
  display (FunctionDefinitionTagStateMutability s) = display s
  display (FunctionDefinitionTagPublic _) = "public"
  display (FunctionDefinitionTagPrivate _) = "private"

  parser =
    choice
      [ try $ addSource $ FunctionDefinitionTagPublic <$ keyword "public",
        try $ addSource $ FunctionDefinitionTagPrivate <$ keyword "private",
        try $ FunctionDefinitionTagStateMutability <$> parser,
        try $ FunctionDefinitionTagModifierInvocation <$> parser
      ]

-------------------------------------------------------------------------------
-- EnumValue = Identifier

type EnumValue = Identifier

-------------------------------------------------------------------------------
-- IndexedParameterList =
--  '(' ( TypeName 'indexed'? Identifier? (',' TypeName 'indexed'? Identifier?)* )? ')'

instance Parseable (IndexedParameterList SourceRange) where
  display (IndexedParameterList ps) = "(" ++ intercalate ", " (map display ps) ++ ")"
  parser = IndexedParameterList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')

instance Parseable (IndexedParameter SourceRange) where
  display ip =
    display (indexedParameterType ip)
      ++ (if indexedParameterIndexed ip then " indexed" else "")
      ++ maybe "" _display (indexedParameterIdentifier ip)
  parser =
    do
      start <- getPosition
      paramType' <- parser <* whitespace
      indexed' <- (try (True <$ keyword "indexed") <|> return False) <* whitespace
      identifier' <- parser
      IndexedParameter paramType' indexed' identifier' . SourceRange start <$> getPosition

-------------------------------------------------------------------------------
-- UntypedParameterList = '(' ( Identifier (',' Identifier)* )? ')'

instance Parseable (UntypedParameterList SourceRange) where
  display (UntypedParameterList ps) = "(" ++ intercalate ", " (map display ps) ++ ")"
  parser = UntypedParameterList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')

-------------------------------------------------------------------------------
-- ParameterList = '(' ( TypeName Identifier? (',' TypeName Identifier?)* )? ')'

instance Parseable (ParameterList SourceRange) where
  display (ParameterList ps) = "(" ++ intercalate ", " (map display ps) ++ ")"
  parser = ParameterList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')

instance Parseable (Parameter SourceRange) where
  display p =
    display (parameterType p)
      ++ maybe "" _display (parameterStorageLocation p)
      ++ maybe "" _display (parameterIdentifier p)
  parser =
    do
      start <- getPosition
      paramType' <- parser <* whitespace
      storageLocation' <- (try (Just <$> parser) <|> return Nothing) <* whitespace
      identifier' <- try (Just <$> parser) <|> return Nothing
      Parameter paramType' storageLocation' identifier' . SourceRange start <$> getPosition

-------------------------------------------------------------------------------
-- TypeNameList = '(' ( TypeName (',' TypeName )* )? ')'

instance Parseable (TypeNameList SourceRange) where
  display (TypeNameList ts) = "(" ++ intercalate ", " (map display ts) ++ ")"
  parser = TypeNameList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')

-------------------------------------------------------------------------------
-- VariableDeclaration = TypeName StorageLocation? Identifier

instance Parseable (VariableDeclaration SourceRange) where
  display v =
    display (variableDeclarationType v)
      ++ maybe "" _display (variableDeclarationStorageLocation v)
      ++ _display (variableDeclarationName v)
  parser =
    do
      t <- parser <* whitespace
      sl <- parser <* whitespace
      i <- parser
      return $ VariableDeclaration t sl i $ mergeRange (ann t) (ann i)

-------------------------------------------------------------------------------
-- TypeName
--          = 'mapping' '(' ElementaryTypeName '=>' TypeName ')'
--          | ElementaryTypeName
--          | 'function' TypeNameList ( StateMutability )* ( 'returns' TypeNameList )?
--          | UserDefinedTypeName

--          | TypeName '[' Expression? ']'

instance Parseable (TypeName SourceRange) where
  display (TypeNameMapping t1 t2 _) = "mapping (" ++ display t1 ++ " => " ++ display t2 ++ ")"
  display (TypeNameElementaryTypeName t _) = display t
  display (TypeNameUserDefinedTypeName t _) = display t
  display (TypeNameArrayTypeName t me _) = display t ++ "[" ++ maybe "" display me ++ "]"
  display (TypeNameFunctionTypeName tl ms mtl' _) =
    "function " ++ display tl ++ (if null ms then "" else " " ++ unwords (map display ms))
      ++ maybe "" (\r -> " returns " ++ display r) mtl'

  parser =
    do
      start <- getPosition
      t <- addSource parserBasic <* whitespace
      mes <- many (try $ whitespace *> parseArrayBrackets)
      end <- getPosition <* whitespace1
      let a =
            if null mes
              then ann t
              else SourceRange start end
      return $ construct t mes a
    where
      parserBasic =
        choice
          [ do
              t1 <- keyword "mapping" *> whitespace *> char '(' *> parser <* whitespace <* string "=>" <* whitespace
              t2 <- parser <* whitespace <* char ')'
              return (TypeNameMapping t1 t2),
            try $ TypeNameElementaryTypeName <$> parser,
            try $ do
              tl <- keyword "function" *> whitespace *> parser <* whitespace
              ms <- many (parser <* whitespace)
              mtl' <- try (Just <$> keyword "returns" *> whitespace *> parser) <|> return Nothing
              return (TypeNameFunctionTypeName tl ms mtl'),
            TypeNameUserDefinedTypeName <$> parser
          ]

      parseArrayBrackets =
        char '[' *> whitespace
          *> do
            char ']' $> Nothing <|> Just <$> parser <* whitespace <* char ']'

      construct t [] _ = t
      construct t (me : mes) a = construct (TypeNameArrayTypeName t me a) mes a

-------------------------------------------------------------------------------
-- UserDefinedTypeName = Identifier ( '.' Identifier )*

instance Parseable (UserDefinedTypeName SourceRange) where
  display (UserDefinedTypeName is) = intercalate "." (map display is)
  parser = UserDefinedTypeName <$> sep1 '.' parser

-------------------------------------------------------------------------------
-- StorageLocation = 'memory' | 'storage'

instance Parseable (StorageLocation SourceRange) where
  display (StorageLocation Memory _) = "memory"
  display (StorageLocation Storage _) = "storage"
  display (StorageLocation CallData _) = "calldata"
  parser = addSource $ StorageLocation <$> p
    where
      p =
        choice
          [ Memory <$ keyword "memory",
            Storage <$ keyword "storage",
            CallData <$ keyword "calldata"
          ]

-------------------------------------------------------------------------------
-- StateMutability = 'internal' | 'external' | 'pure' | 'constant' | 'view' | 'payable'

instance Parseable (StateMutability SourceRange) where
  display (StateMutability Pure _) = "pure"
  display (StateMutability Internal _) = "internal"
  display (StateMutability External _) = "external"
  display (StateMutability Constant _) = "constant"
  display (StateMutability View _) = "view"
  display (StateMutability Payable _) = "payable"

  parser = addSource $ StateMutability <$> p
    where
      p =
        choice
          [ Internal <$ keyword "internal",
            External <$ keyword "external",
            Constant <$ keyword "constant",
            View <$ keyword "view",
            char 'p'
              *> (Pure <$ keyword "ure" <|> Payable <$ keyword "ayable")
          ]

-------------------------------------------------------------------------------
-- IdentifierList = '(' ( Identifier? ',' )* Identifier? ')'

instance Parseable (IdentifierList SourceRange) where
  display (IdentifierList is) =
    "(" ++ intercalate ", " (map display is) ++ ")"
  parser = IdentifierList <$> (char '(' *> whitespace *> commaSep (parser <* whitespace) <* char ')')

-------------------------------------------------------------------------------
-- Block = '{' Statement* '}'

instance Parseable (Block SourceRange) where
  display (Block ss _) = "{\n" ++ indent (intercalate "\n" (map display ss)) ++ "}"
  parser = addSource $ Block <$> (char '{' *> whitespace *> many (parser <* whitespace) <* char '}')

-------------------------------------------------------------------------------
-- Statement = IfStatement | WhileStatement | ForStatement | Block | InlineAssemblyStatement |
--             ( DoWhileStatement | PlaceholderStatement | Continue | Break | Return |
--               Throw | SimpleStatement ) ';'
--
-- IfStatement = 'if' '(' Expression ')' Statement ( 'else' Statement )?
-- WhileStatement = 'while' '(' Expression ')' Statement
-- InlineAssemblyStatement = 'assembly' StringLiteral? InlineAssemblyBlock
-- ForStatement = 'for' '(' (SimpleStatement)? ';' (Expression)? ';' (Expression)? ')' Statement
--
-- DoWhileStatement = 'do' Statement 'while' '(' Expression ')'
-- PlaceholderStatement = '_'
-- Continue = 'continue'
-- Break = 'break'
-- Return = 'return' Expression?
-- Throw = 'throw'
-- EmitStatement = 'emit' Expression
-- SimpleStatement =
--    Expression | ('var' IdentifierList ( '=' Expression ) | VariableDeclaration ( '=' Expression )?


instance Parseable (Statement SourceRange) where
  display (IfStatement e t me _) =
    "if (" ++ display e ++ ") " ++ display t
      ++ maybe
        ""
        ( \s ->
            case s of
              BlockStatement (Block [] _) -> ""
              _ -> " else " ++ display s
        )
        me
  display (WhileStatement e s _) = "while (" ++ display e ++ ") " ++ display s
  display (InlineAssemblyStatement ms b _) = "assembly " ++ maybe " " (\s -> display s ++ " ") ms ++ display b
  display (ForStatement (ms, me1, me2) s _) =
    "for ("
      ++ maybe "; " (\ss -> display ss ++ " ") ms
      ++ maybe "" display me1
      ++ "; "
      ++ maybe "" display me2
      ++ ") "
      ++ display s
  display (BlockStatement b) = display b
  display (DoWhileStatement s e _) = "do " ++ display s ++ " while (" ++ display e ++ ");"
  display (PlaceholderStatement _) = "_;"
  display (RevertStatement e _) = "revert "++display e++";"
  display (Continue _) = "continue;"
  display (Break _) = "break;"
  display (Return me _) = "return" ++ maybe "" (\e -> " " ++ display e) me ++ ";"
  display (Throw _) = "throw;"
  display (EmitStatement e _) = "emit " ++ display e ++ ";"
  display (SimpleStatementExpression e _) = display e ++ ";"
  display (SimpleStatementVariableList il me _) = "var " ++ display il ++ maybe "" (\e -> " = " ++ display e) me ++ ";"
  -- display (SimpleStatementVariableDeclaration v me) = display v ++ maybe "" (\e -> " = "++display e) me ++";"
  display (SimpleStatementVariableDeclarationList [v] [] _) = display v ++ ";"
  display (SimpleStatementVariableDeclarationList [v] [d] _) = display v ++ " = " ++ display d ++ ";"
  display (SimpleStatementVariableDeclarationList vs [] _) = "(" ++ intercalate ", " (map display vs) ++ ")" ++ ";"
  display (SimpleStatementVariableDeclarationList vs me _) = "(" ++ intercalate ", " (map display vs) ++ ")" ++ " = " ++ "(" ++ intercalate ", " (map display me) ++ ")" ++ ";"
  display (SimpleStatementVariableAssignmentList [v] [] _) = display v ++ ";"
  display (SimpleStatementVariableAssignmentList [v] [d] _) = display v ++ " = " ++ display d ++ ";"
  display (SimpleStatementVariableAssignmentList vs [] _) = "(" ++ intercalate ", " (map display vs) ++ ")" ++ ";"
  display (SimpleStatementVariableAssignmentList vs me _) = "(" ++ intercalate ", " (map display vs) ++ ")" ++ " = " ++ "(" ++ intercalate ", " (map display me) ++ ")" ++ ";"

  parser =
    try
      ( choice
          [ do
              start <- getPosition
              s <- keyword "do" *> whitespace *> parser <* whitespace <* keyword "while" <* whitespace <* char '(' <* whitespace
              e <- parser <* whitespace <* char ')' <* whitespace <* char ';'
              DoWhileStatement s e . SourceRange start <$> getPosition,
            do
              start <- getPosition
              _ <- char '_' <* whitespace <* char ';'
              PlaceholderStatement . SourceRange start <$> getPosition,
            do
              start <- getPosition
              _ <- keyword "continue" <* whitespace <* char ';'
              Continue . SourceRange start <$> getPosition,
            do
              start <- getPosition
              _ <- keyword "break" <* whitespace <* char ';'
              Break . SourceRange start <$> getPosition,
            do
              start <- getPosition
              _ <- keyword "throw" <* whitespace <* char ';'
              Throw . SourceRange start <$> getPosition,
            try (do
              start <- getPosition
              e <- keyword "emit" *> whitespace *> parser <* whitespace <* char ';'
              EmitStatement e . SourceRange start <$> getPosition),
            try (do
              start <- getPosition
              e <- keyword "revert" *> whitespace *> parser <* whitespace <* char ';'
              RevertStatement e . SourceRange start <$> getPosition),
            try (do
              start <- getPosition
              e <- keyword "return" *> whitespace *> (Just <$> parser <|> return Nothing) <* whitespace <* char ';'
              Return e . SourceRange start <$> getPosition),
            do
              start <- getPosition
              c <- keyword "if" *> whitespace *> char '(' *> whitespace *> parser <* whitespace <* char ')' <* whitespace
              t <- parser <* whitespace
              e <- Just <$> keyword "else" *> whitespace *> parser <|> return Nothing
              IfStatement c t e . SourceRange start <$> getPosition,
            do
              start <- getPosition
              c <- keyword "while" *> whitespace *> char '(' *> whitespace *> parser <* whitespace <* char ')' <* whitespace
              s <- parser
              WhileStatement c s . SourceRange start <$> getPosition,
            BlockStatement <$> parser,
            do
              start <- getPosition
              n <- keyword "assembly" *> whitespace *> (Just <$> parser <|> return Nothing) <* whitespace
              b <- parser
              InlineAssemblyStatement n b . SourceRange start <$> getPosition,
            do
              start <- getPosition
              s1 <-
                keyword "for" *> whitespace *> char '(' *> whitespace
                  *> (try (Just <$> parseSimpleStatement) <|> char ';' $> Nothing) <* whitespace
              s2 <- (try (Just <$> parser) <|> return Nothing) <* whitespace <* char ';' <* whitespace
              s3 <- (try (Just <$> parser) <|> return Nothing) <* whitespace <* char ')' <* whitespace
              b <- parser
              ForStatement (s1, s2, s3) b . SourceRange start <$> getPosition
          ]
      )
      <|> parseSimpleStatement
    where
      parseSimpleStatement =
        try
          ( do
              start <- getPosition
              il <- keyword "var" *> whitespace *> parser <* whitespace <* char '=' <* whitespace
              me <- try (Just <$> parser) <|> return Nothing
              _ <- whitespace *> char ';'
              SimpleStatementVariableList il me . SourceRange start <$> getPosition
          )
          <|>
          -- try (
          --   do
          --     vd <- try (char '(' *> whitespace *> parser <* whitespace <* char ')') <|> parser
          --     whitespace
          --     me <- try (Just <$> (char '=' *> whitespace *> parser)) <|> return Nothing
          --     _  <- whitespace *> char ';'
          --     return (SimpleStatementVariableDeclaration vd me)
          --   )
          -- <|>
          try
            ( do
                start <- getPosition
                char '('
                whitespace
                char ')'
                whitespace
                char ';'
                SimpleStatementVariableDeclarationList [] [] . SourceRange start <$> getPosition
            )
          <|> try
            ( do
                start <- getPosition
                optional (char '(') <* whitespace
                vd <- commaSep1 (try (Just <$> whitespace *> parser <* whitespace) <|> char ' ' *> whitespace $> Nothing)
                optional (char ')') <* whitespace
                r <-
                  try
                    ( do
                        char '=' <* whitespace
                        me <- whitespace *> parser <* whitespace
                        whitespace <* char ';'
                        return [me]
                    )
                    <|> try
                      ( do
                          char '=' <* whitespace
                          optional (char '(')
                          mes <- try (commaSep1 (whitespace *> parser <* whitespace)) <|> return []
                          optional (char ')')
                          whitespace <* char ';'
                          return mes
                      )
                    <|> [] <$ whitespace <* char ';'
                SimpleStatementVariableDeclarationList vd r . SourceRange start <$> getPosition
            )
          <|> try
            ( do
                start <- getPosition
                optional (char '(') <* whitespace
                vd <- commaSep1 (try (Just <$> whitespace *> parser <* whitespace) <|> char ' ' *> whitespace $> Nothing)
                optional (char ')') <* whitespace
                r <-
                  try
                    ( do
                        char '=' <* whitespace
                        me <- whitespace *> parser <* whitespace
                        whitespace <* char ';'
                        return [me]
                    )
                    <|> try
                      ( do
                          char '=' <* whitespace
                          optional (char '(')
                          mes <- try (commaSep1 (whitespace *> parser <* whitespace)) <|> return []
                          optional (char ')')
                          whitespace <* char ';'
                          return mes
                      )
                -- <|> return [] <$> whitespace <* char ';'
                SimpleStatementVariableAssignmentList vd r . SourceRange start <$> getPosition
            )
          <|> try
            ( do
                start <- getPosition
                e <- parser <* whitespace <* char ';'
                SimpleStatementExpression e . SourceRange start <$> getPosition
            )

-------------------------------------------------------------------------------
--  Precedence by order (see github.com/ethereum/solidity/pull/732)
-- Expression
--    = Expression ('++' | '--')
--    | Expression '[' Expression? ']'                                         -- index access
--    | '(' Expression ')'
--    | ('!' | '~' | 'delete' | '++' | '--' | '+' | '-') Expression
--    | Expression '**' Expression
--    | Expression ('*' | '/' | '%') Expression
--    | Expression ('+' | '-') Expression
--    | Expression ('<<' | '>>') Expression
--    | Expression '&' Expression
--    | Expression '^' Expression
--    | Expression '|' Expression
--    | Expression ('<' | '>' | '<=' | '>=') Expression
--    | Expression ('==' | '!=') Expression
--    | Expression '&&' Expression
--    | Expression '||' Expression
--    | Expression '?' Expression ':' Expression
--    | Expression ('=' | '|=' | '^=' | '&=' | '<<=' | '>>=' | '+=' | '-=' | '*=' | '/=' | '%=') Expression

--   | PrimaryExpression
--   | Expression '(' ('{' NameValueList? '}' | ExpressionList? ) ')'         -- function call
--   | Expression '.' Identifier                                              -- member access
--   | 'new' Typename

instance Parseable (Expression SourceRange) where
  display (New t _) = "new " ++ display t
  display (MemberAccess e i _) = display e ++ "." ++ display i
  display (Literal e) = display e
  display (FunctionCallNameValueList e mvs _) = display e ++ "({" ++ maybe "" display mvs ++ "})"
  display (FunctionCallExpressionList e mvs _) = display e ++ "(" ++ maybe "" display mvs ++ ")"
  display (Unary (Operator "delete" _) e _) = "delete " ++ display e
  display (Unary (Operator "()++" _) e _) = display e ++ "++"
  display (Unary (Operator "()--" _) e _) = "(" ++ display e ++ ")--"
  display (Unary (Operator "()" _) e _) = "(" ++ display e ++ ")"
  display (Unary (Operator "[]" _) e _) = display e ++ "[]"
  -- Remaining: ! ~ + - ++ --
  display (Unary (Operator op _) e _) = op ++ display e
  display (Binary (Operator "[]" _) e1 e2 _) = display e1 ++ "[" ++ display e2 ++ "]"
  -- Remaining = |= ^= &= <<= >>= += -= *= /= %= || && == != <= >= < > + - * ** / % ^ | & >> <<
  display (Binary (Operator op _) e1 e2 _) = display e1 ++ " " ++ op ++ " " ++ display e2
  display (Ternary (Operator op _) e1 e2 e3 _) = display e1 ++ op ++ display e2 ++ ":" ++ display e3

  parser = parserPrec (15 :: Integer)
    where
      anyString ss = choice (map (try . string) (init ss) ++ [string $ last ss])
      binaryOperators n ops =
        do
          p1 <- parserPrec (n -1) <* whitespace
          try
            ( do
                op <- addSource $ Operator <$> anyString ops
                p2 <- whitespace *> parserPrec n
                return (Binary op p1 p2 $ mergeRange (ann p1) (ann p2))
            )
            <|> return p1

      parserPrec 15 = binaryOperators 15 ["=", "|=", "^=", "&=", "<<=", ">>=", "+=", "-=", "*=", "/=", "%="]
      parserPrec 14 =
        try
          ( do
              p1 <- parserPrec 13 <* whitespace
              op <- addSource $ Operator <$> anyString ["?"]
              p2 <- whitespace *> parserPrec 13 <* whitespace
              p3 <- char ':' *> whitespace *> parserPrec 13
              return (Ternary op p1 p2 p3 $ mergeRange (ann p1) (ann p3))
          )
          <|> parserPrec 13
      parserPrec 13 = binaryOperators 13 ["||"]
      parserPrec 12 = binaryOperators 12 ["&&"]
      parserPrec 11 = binaryOperators 11 ["==", "!="]
      parserPrec 10 = binaryOperators 10 ["<=", ">=", "<", ">"]
      parserPrec 9 = binaryOperators 9 ["|"]
      parserPrec 8 = binaryOperators 8 ["^"]
      parserPrec 7 = binaryOperators 7 ["&"]
      parserPrec 6 = binaryOperators 6 ["<<", ">>"]
      parserPrec 5 = binaryOperators 5 ["+", "-"]
      parserPrec 4 = binaryOperators 4 ["*", "/", "%"]
      parserPrec 3 = binaryOperators 3 ["**"]
      parserPrec 2 =
        try
          ( do
              start <- getPosition
              op <- addSource $ Operator <$> (anyString ["++", "--", "+", "-", "!", "~"] <|> keyword "delete")
              p <- whitespace *> parserPrec 15
              return (Unary op p $ (ann p) {srcStart = start})
          )
          <|> parserPrec 1
      -- -- parserPrec 1 split into two to avoid infinite loops
      parserPrec 1 =
        choice
          [ try $ do
              start <- getPosition
              p <- parserPrec 0 <* whitespace
              (Operator op a) <- addSource $ Operator <$> anyString ["++", "--"]
              return (Unary (Operator ('(' : ')' : op) a) p $ (ann p) {srcStart = start})
          ]
          <|> parserPrec 0
      parserPrec 0 =
        do
          p1 <- parserPrecBasic <* whitespace
          addBottomLevelOperators p1
        where
          addBottomLevelOperators :: Expression SourceRange -> Parser (Expression SourceRange)
          addBottomLevelOperators p1 =
            choice
              [ try $ do
                  start <- getPosition
                  p2 <- char '[' *> whitespace *> parserPrec 15 <* whitespace <* char ']'
                  end <- getPosition
                  addBottomLevelOperators (Binary (Operator "[]" $ SourceRange start end) p1 p2 $ mergeRange (ann p1) (ann p2)),
                try
                  ( do
                      i <- char '.' *> whitespace *> parser
                      addBottomLevelOperators (MemberAccess p1 i $ mergeRange (ann p1) (ann i))
                  ),
                try
                  ( do
                      start <- getPosition
                      _ <- char '(' <* whitespace
                      result <-
                        char '{' *> whitespace *> (FunctionCallNameValueList p1 <$> (parser <* whitespace <* char '}' <* whitespace <* char ')'))
                          <|> FunctionCallExpressionList p1 <$> (parser <* whitespace <* char ')')
                      end <- getPosition
                      addBottomLevelOperators (result $ mergeRange (ann p1) $ SourceRange start end)
                  ),
                return p1
              ]
          parserPrecBasic :: Parser (Expression SourceRange)
          parserPrecBasic =
            choice
              [ try $ addSource $ New <$> (keyword "new" *> whitespace *> parser),
                try $ do
                  start <- getPosition
                  expr <- char '(' *> whitespace *> parserPrec 15 <* whitespace <* char ')'
                  end <- getPosition
                  let sr = SourceRange start end
                  return $ Unary (Operator "()" sr) expr sr,
                Literal <$> parser
              ]
      parserPrec x = error $ "Invalid param value `" ++ show x ++ "` for function `parserPrec`"

-------------------------------------------------------------------------------
-- PrimaryExpression = BooleanLiteral
--                   | NumberLiteral
--                   | HexLiteral
--                   | StringLiteral
--                   | TupleExpression
--                   | Identifier
--                   | ElementaryTypeNameExpression

instance Parseable (PrimaryExpression SourceRange) where
  parser =
    choice
      [ try $ PrimaryExpressionBooleanLiteral <$> parser,
        try $ PrimaryExpressionNumberLiteral <$> parser,
        try $ PrimaryExpressionHexLiteral <$> parser,
        try $ PrimaryExpressionStringLiteral <$> parser,
        try $ PrimaryExpressionTupleExpression <$> parser,
        try $ PrimaryExpressionIdentifier <$> parser,
        PrimaryExpressionElementaryTypeNameExpression <$> parser
      ]
  display (PrimaryExpressionBooleanLiteral l) = display l
  display (PrimaryExpressionNumberLiteral l) = display l
  display (PrimaryExpressionHexLiteral l) = display l
  display (PrimaryExpressionStringLiteral l) = display l
  display (PrimaryExpressionTupleExpression l) = display l
  display (PrimaryExpressionIdentifier l) = display l
  display (PrimaryExpressionElementaryTypeNameExpression l) = display l

-------------------------------------------------------------------------------
-- ExpressionList = Expression ( ',' Expression )*

instance Parseable (ExpressionList SourceRange) where
  parser = ExpressionList <$> commaSep1 parser
  display (ExpressionList es) = intercalate ", " (map display es)

-------------------------------------------------------------------------------
-- NameValueList = Identifier ':' Expression ( ',' Identifier ':' Expression )*

instance Parseable (NameValueList SourceRange) where
  parser =
    NameValueList
      <$> commaSep1
        ( do i <- parser <* whitespace <* char ':' <* whitespace; e <- parser; return (i, e)
        )
  display (NameValueList ies) = intercalate ", " $ map (\(i, e) -> display i ++ ":" ++ display e) ies

-------------------------------------------------------------------------------
-- BooleanLiteral = 'true' | 'false'

instance Parseable (BooleanLiteral SourceRange) where
  parser = addSource $ BooleanLiteral <$> (string "true" <|> string "false")
  display (BooleanLiteral lit _) = lit

-------------------------------------------------------------------------------
-- NumberLiteral = ( HexNumber | DecimalNumber ) (' ' NumberUnit)?

-- HexNumber = '0x' [0-9a-fA-F]+
-- DecimalNumber = [0-9]+

instance Parseable (NumberLiteral SourceRange) where
  parser =
    addSource $
      NumberLiteral
        <$> ( try
                ( do
                    n <- string "0x" *> many1 (digit <|> oneOf "ABCDEFabcdef")
                    NumberLiteralHex n <$> parseMaybeUnits
                )
                <|> do
                  n <- many1 digit
                  NumberLiteralDec n <$> parseMaybeUnits
            )
    where
      parseMaybeUnits = try (Just <$> char ' ' *> whitespace *> parser) <|> return Nothing

  display (NumberLiteral (NumberLiteralHex n units) _) = "0x" ++ n ++ maybe "" _display units
  display (NumberLiteral (NumberLiteralDec n units) _) = n ++ maybe "" _display units

-------------------------------------------------------------------------------
-- NumberUnit = 'wei' | 'szabo' | 'finney' | 'ether'
--           | 'seconds' | 'minutes' | 'hours' | 'days' | 'weeks' | 'years'

instance Parseable NumberUnit where
  parser =
    choice
      [ kw "finney" Finney,
        kw "ether" Ether,
        kw "years" Years,
        kw "minutes" Minutes,
        kw "hours" Hours,
        kw "days" Days,
        char 's' *> (kw "econds" Seconds <|> kw "zabo" Szabo),
        string "we" *> (kw "eks" Weeks <|> kw "i" Wei)
      ]
    where
      kw s v = keyword s $> v
  display = map toLower . show

-------------------------------------------------------------------------------
-- HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')

instance Parseable (HexLiteral SourceRange) where
  parser =
    addSource $
      HexLiteral . concat
        <$> ( string "hex"
                *> try (char '"' *> manyTill parseHexByte (char '"'))
                <|> char '\''
                *> manyTill parseHexByte (char '\'')
            )
    where
      parseHexChar = digit <|> oneOf "ABCDEFabcdef"
      parseHexByte = list [parseHexChar, parseHexChar]

  display (HexLiteral hl _) = "hex'" ++ hl ++ "'"

-------------------------------------------------------------------------------
-- StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'

instance Parseable (StringLiteral SourceRange) where
  parser =
    addSource $
      StringLiteral . concat
        <$> ( try (char '"' *> manyTill character (char '"'))
                <|> char '\'' *> manyTill character (char '\'')
            )
    where
      escape :: Parser String
      escape = list [char '\\', oneOf ['\\', '\"', '0', 'n', 'r', 'v', 't', 'b', 'f', 'x', 'u']] -- all the characters which can be escaped
      nonEscape :: Parser Char
      nonEscape = noneOf ['\\', '\"', '\0', '\n', '\r', '\v', '\t', '\b', '\f']

      character :: Parser String
      character = try (return <$> nonEscape) <|> escape
  display (StringLiteral s _) = '"' : addEscapes s ++ "\""
    where
      addEscapes = concatMap addEscape
      addEscape '\n' = "\\n"
      addEscape '\0' = "\\0"
      --  addEscape '\\' = "\\\\"
      addEscape '"' = "\""
      addEscape '\r' = "\\r"
      addEscape '\v' = "\\v"
      addEscape '\t' = "\\t"
      addEscape '\b' = "\\b"
      addEscape '\f' = "\\f"
      addEscape c = [c]

-------------------------------------------------------------------------------
-- Identifier = [a-zA-Z_$] [a-zA-Z_$0-9]*

instance Parseable (Identifier SourceRange) where
  parser =
    addSource $
      Identifier
        <$> ( (:)
                <$> (letter <|> oneOf "_$")
                <*> many (alphaNum <|> oneOf "_$")
                <* endOfWord
            )
  display (Identifier ident _) = ident

-- -------------------------------------------------------------------------------
-- TupleExpression = '(' ( Expression ( ',' Expression )*  )? ')'
--                 | '[' ( Expression ( ',' Expression )*  )? ']'

instance Parseable (TupleExpression SourceRange) where
  parser =
    addSource
      ( RoundBrackets <$> (char '(' *> whitespace *> commaSep (parser <* whitespace) <* char ')')
          <|> SquareBrackets <$> (char '[' *> whitespace *> commaSep1 (parser <* whitespace) <* char ']')
      )
  display (RoundBrackets es _) = "(" ++ intercalate ", " (map display es) ++ ")"
  display (SquareBrackets es _) = "[" ++ intercalate ", " (map display es) ++ "]"

-- -------------------------------------------------------------------------------
-- ElementaryTypeNameExpression = ElementaryTypeName

type ElementaryTypeNameExpression = ElementaryTypeName

-- -------------------------------------------------------------------------------
-- ElementaryTypeName = 'address' | 'bool' | 'string' | 'var'
--                    | Int | Uint | Byte | Fixed | Ufixed
--
-- Int = 'int' | 'int8' | 'int16' | 'int24' | 'int32' | 'int40' | 'int48' | 'int56' | 'int64' | 'int72' | 'int80' | 'int88' | 'int96' | 'int104' | 'int112' | 'int120' | 'int128' | 'int136' | 'int144' | 'int152' | 'int160' | 'int168' | 'int176' | 'int184' | 'int192' | 'int200' | 'int208' | 'int216' | 'int224' | 'int232' | 'int240' | 'int248' | 'int256'
-- Uint = 'uint' | 'uint8' | 'uint16' | 'uint24' | 'uint32' | 'uint40' | 'uint48' | 'uint56' | 'uint64' | 'uint72' | 'uint80' | 'uint88' | 'uint96' | 'uint104' | 'uint112' | 'uint120' | 'uint128' | 'uint136' | 'uint144' | 'uint152' | 'uint160' | 'uint168' | 'uint176' | 'uint184' | 'uint192' | 'uint200' | 'uint208' | 'uint216' | 'uint224' | 'uint232' | 'uint240' | 'uint248' | 'uint256'
-- Byte = 'byte' | 'bytes' | 'bytes1' | 'bytes2' | 'bytes3' | 'bytes4' | 'bytes5' | 'bytes6' | 'bytes7' | 'bytes8' | 'bytes9' | 'bytes10' | 'bytes11' | 'bytes12' | 'bytes13' | 'bytes14' | 'bytes15' | 'bytes16' | 'bytes17' | 'bytes18' | 'bytes19' | 'bytes20' | 'bytes21' | 'bytes22' | 'bytes23' | 'bytes24' | 'bytes25' | 'bytes26' | 'bytes27' | 'bytes28' | 'bytes29' | 'bytes30' | 'bytes31' | 'bytes32'
-- Fixed = 'fixed' | ( 'fixed' DecimalNumber 'x' DecimalNumber )
-- Ufixed = 'ufixed' | ( 'ufixed' DecimalNumber 'x' DecimalNumber )

instance Parseable (ElementaryTypeName SourceRange) where
  display (ElementaryTypeName AddressPayableType _) = "address payable"
  display (ElementaryTypeName AddressType _) = "address"
  display (ElementaryTypeName BoolType _) = "bool"
  display (ElementaryTypeName StringType _) = "string"
  display (ElementaryTypeName VarType _) = "var"
  display (ElementaryTypeName ByteType _) = "byte"
  display (ElementaryTypeName (IntType Nothing) _) = "int"
  display (ElementaryTypeName (IntType (Just n)) _) = "int" ++ show n
  display (ElementaryTypeName (UintType Nothing) _) = "uint"
  display (ElementaryTypeName (UintType (Just n)) _) = "uint" ++ show n
  display (ElementaryTypeName (BytesType Nothing) _) = "bytes"
  display (ElementaryTypeName (BytesType (Just n)) _) = "bytes" ++ show n
  display (ElementaryTypeName (FixedType Nothing) _) = "fixed"
  display (ElementaryTypeName (FixedType (Just (d1, d2))) _) = "fixed" ++ show d1 ++ "x" ++ show d2
  display (ElementaryTypeName (UfixedType Nothing) _) = "ufixed"
  display (ElementaryTypeName (UfixedType (Just (d1, d2))) _) = "ufixed" ++ show d1 ++ "x" ++ show d2

  parser = addSource $ ElementaryTypeName <$> p
    where
      p =
        choice
          [ try (AddressPayableType <$ keyword "address" <* whitespace <* keyword "payable") <|> AddressType <$ keyword "address",
            StringType <$ keyword "string",
            VarType <$ keyword "var",
            IntType <$> (string "int" *> parseIntSize),
            FixedType <$> (string "fixed" *> parseFixedPair),
            char 'u'
              *> ( UfixedType <$> (string "fixed" *> parseFixedPair)
                     <|> UintType <$> (string "int" *> parseIntSize)
                 ),
            char 'b'
              *> ( BoolType <$ keyword "ool"
                     <|> string "yte"
                       *> ( BytesType <$> (char 's' *> parseBytesSize)
                              <|> return ByteType
                          )
                 )
          ]

      parseIntSize :: Parser (Maybe Integer)
      parseIntSize =
        do
          ns <- many digit
          let n = read ns :: Integer
          if not (null ns) && n `mod` 8 == 0 && n >= 8 && n <= 256 then return (Just n) else mzero
          <|> return Nothing
      parseBytesSize =
        do
          ns <- many digit
          let n = read ns :: Integer
          if not (null ns) && n > 0 && n >= 1 && n <= 32 then return (Just n) else mzero
          <|> return Nothing
      parseFixedPair =
        try
          ( do
              d1 <- many digit <* char 'x'
              d2 <- many digit
              if null d1 || null d2 then mzero else return (Just (read d1, read d2))
          )
          <|> return Nothing

-- -------------------------------------------------------------------------------
-- InlineAssemblyBlock = '{' AssemblyItem* '}'

instance Parseable (InlineAssemblyBlock SourceRange) where
  display (InlineAssemblyBlock is) = "{ " ++ unlines (map display is) ++ " }"
  parser = InlineAssemblyBlock <$> (char '{' *> whitespace *> many (parser <* whitespace) <* char '}')

-- -------------------------------------------------------------------------------
-- AssemblyItem = Identifier | FunctionalAssemblyExpression | InlineAssemblyBlock | AssemblyLocalBinding | AssemblyAssignment | AssemblyLabel | NumberLiteral | StringLiteral | HexLiteral
-- AssemblyLabel = Identifier ':'
-- AssemblyLocalBinding = 'let' Identifier ':=' FunctionalAssemblyExpression
-- AssemblyAssignment = ( Identifier ':=' FunctionalAssemblyExpression ) | ( '=:' Identifier )

instance Parseable (AssemblyItem SourceRange) where
  parser =
    choice
      [ try $ AssemblyItemFunctionalAssemblyExpression <$> parser,
        try $ AssemblyItemInlineAssemblyBlock <$> parser,
        try $ do _ <- keyword "let" <* whitespace; i <- parser <* whitespace <* string ":=" <* whitespace; AssemblyItemAssemblyLocalBinding i <$> parser,
        try $ do i <- parser <* whitespace <* string ":=" <* whitespace; AssemblyItemAssemblyAssignment i <$> parser,
        try $ do e <- parser <* whitespace <* string "=:" <* whitespace; i <- parser; return (AssemblyItemAssemblyAssignment i e),
        try $ AssemblyItemAssemblyLabel <$> parser <* char ':',
        try $ AssemblyItemNumberLiteral <$> parser,
        try $ AssemblyItemStringLiteral <$> parser,
        try $ AssemblyItemHexLiteral <$> parser,
        AssemblyItemIdentifier <$> parser
      ]
  display (AssemblyItemStringLiteral s) = display s
  display (AssemblyItemHexLiteral h) = display h
  display (AssemblyItemNumberLiteral n) = display n
  display (AssemblyItemAssemblyLabel l) = display l ++ ":"
  display (AssemblyItemIdentifier i) = display i
  display (AssemblyItemAssemblyLocalBinding i e) = "let " ++ display i ++ " := " ++ display e
  display (AssemblyItemAssemblyAssignment i e) = display i ++ " := " ++ display e
  display (AssemblyItemInlineAssemblyBlock b) = display b
  display (AssemblyItemFunctionalAssemblyExpression e) = display e

-- -------------------------------------------------------------------------------
-- FunctionalAssemblyExpression = Identifier '(' AssemblyItem? ( ',' AssemblyItem )* ')'

instance Parseable (FunctionalAssemblyExpression SourceRange) where
  parser =
    do
      i <- parser <* whitespace <* char '(' <* whitespace
      items <- commaSep (parser <* whitespace) <* char ')'
      return (FunctionalAssemblyExpression i items)

  display (FunctionalAssemblyExpression i items) = display i ++ "(" ++ intercalate ", " (map display items) ++ ")"
