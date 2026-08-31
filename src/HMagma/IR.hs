{-# LANGUAGE OverloadedStrings #-}

module HMagma.IR (genProgramIR) where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T

import HMagma.AST

newtype Ident = Ident Text deriving (Show, Eq)
newtype Temp  = Temp  Int  deriving (Show, Eq) 

data Operand
    = OInt Integer
    | OFloat Double
    | OString Text
    | OBool Bool
    | OVar Ident
    | OTemp Temp
    | ONil
    deriving (Show)

data TAC
    = TAssign Ident Operand
    | TBinOp Temp BinaryOp Operand Operand
    | TReturn Operand
    deriving (Show)

-- State for counter register
type CompilerState = Int
type Compiler a = State CompilerState a

newTemp :: Compiler Temp
newTemp = do
    n <- get
    put (n + 1)
    pure $ (Temp n)


genProgramIR ::[Stmt] -> [TAC]
genProgramIR stmts = evalState (concat <$> traverse genStmt stmts) 0

genExpr :: Expr -> Compiler (Operand, [TAC])
genExpr (ELiteral _ lit) = pure $ genLit lit
    where
        genLit :: Literal -> (Operand, [TAC])
        genLit (LitInt n)    = (OInt n, [])
        genLit (LitFloat f)  = (OFloat f, [])
        genLit (LitString s) = (OString s, [])
        genLit (LitBool b)   = (OBool b, [])
        genLit (LitNil)      = (ONil, [])

genExpr (EVar _ name) = pure (OVar (Ident name), [])
genExpr (EBinary _ op e1 e2) = do
    (op1, tac1) <- genExpr e1
    (op2, tac2) <- genExpr e2
    t <- newTemp
    let inst = TBinOp t op op1 op2
    pure (OTemp t, tac1 ++ tac2 ++ [inst])

genStmt :: Stmt -> Compiler [TAC]
genStmt (SVarDecl _ name expr) = do
    (op, tac) <- genExpr expr
    pure $ tac ++ [TAssign (Ident name) op]
genStmt (SExpr _ expr) = do
    (op, tac) <- genExpr expr
    pure $ tac ++ [TReturn op]