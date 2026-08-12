{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module HMagma.AST where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)
import GHC.Generics (Generic)

data Literal
    = LitInt Integer
    | LitFloat Double
    | LitString Text
    | LitBool Bool
    | LitNil
    deriving (Show, Eq, Generic)

data BinaryOp
    = OpAdd | OpSub | OpMul | OpDiv | OpMod
    | OpEq  | OpNeq | OpLt  | OpLte | OpGt | OpGte
    | OpAnd | OpOr
    deriving (Show, Eq, Generic)

data UnaryOp = OpNeg | OpNot deriving (Show, Eq, Generic)

data Expr
    = ELiteral  !SourcePos !Literal
    | EVar      !SourcePos !Text
    | EUnary    !SourcePos !UnaryOp !Expr
    | EBinary   !SourcePos !BinaryOp !Expr !Expr
    | EIf       !SourcePos !Expr !Expr !Expr 
    | EBlock    !SourcePos ![Stmt] !(Maybe Expr)
    | ELet      !SourcePos !Text !Expr !Expr
    | EApp      !SourcePos !Expr ![Expr]
    | ELambda   !SourcePos ![Text] !Expr
    | ESExpr    !SourcePos !Text ![Expr]
    deriving (Show, Eq, Generic)

data Stmt
    = SExpr     !SourcePos !Expr
    | SVarDecl  !SourcePos !Text !Expr
    | SFuncDef  !SourcePos !Text ![Text] !Expr
    | SWhile    !SourcePos !Expr !Stmt
    | SReturn   !SourcePos !(Maybe Expr)
    deriving (Show, Eq, Generic)
