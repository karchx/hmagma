{-# LANGUAGE OverloadedStrings #-}

module HMagma.HIR
    ( genProgramHIR
    , Ident
    , HIRProg(..)
    , HIRFun(..)
    , HIRLit(..)
    , HIRExpr(..)
    , HIRStmt(..)
    ) where

import Data.Text (Text)

import HMagma.AST

newtype Ident = Ident Text deriving (Show, Eq, Ord)

data HIRProg = HIRProg
    { progFuncs   :: [HIRFun]
    , progGlobals :: [HIRStmt]
    } deriving (Show, Eq)

data HIRFun = HIRFun
    { funName   :: Ident
    , funParams :: [Ident]
    , funBody   :: HIRExpr
    } deriving (Show, Eq)

data HIRLit
    = HInt Integer
    | HFloat Double
    | HString Text
    | HBool Bool
    | HNil
    deriving (Show, Eq)

data HIRExpr
    = HLit HIRLit
    | HVar Ident
    | HAssignExpr Ident HIRExpr
    | HBin BinaryOp HIRExpr HIRExpr
    | HUnary UnaryOp HIRExpr
    | HIf  HIRExpr HIRExpr HIRExpr
    | HBlock [HIRStmt] HIRExpr
    deriving (Show, Eq)

data HIRStmt
    = HAssign Ident HIRExpr
    | HExpr HIRExpr
    | HReturn HIRExpr
    deriving (Show, Eq)


genProgramHIR :: [Stmt] -> HIRProg
genProgramHIR ast =
    let (funcs, globals) = hoist ast
    in HIRProg funcs globals

hoist :: [Stmt] -> ([HIRFun], [HIRStmt])
hoist [] = ([], [])
hoist stmts =
        let (revFuncs, revStmts) = foldl' step ([], []) stmts
        in (reverse revFuncs, reverse revStmts)
    where
        step (funcs, stmts) stmt = case stmt of
            SFuncDef _ name params body ->
                let newFun = HIRFun
                        { funName   = Ident name
                        , funParams = map Ident params
                        , funBody   = genExpr body
                        }
                in (newFun : funcs, stmts)
            othStmt ->
                (funcs, genStmt othStmt : stmts)

genExpr :: Expr -> HIRExpr
genExpr (ELiteral _ lit) = HLit (toHIRLit lit)
    where
        toHIRLit (LitInt n)    = HInt n
        toHIRLit (LitFloat f)  = HFloat f
        toHIRLit (LitString s) = HString s
        toHIRLit (LitBool b)   = HBool b
        toHIRLit LitNil        = HNil
genExpr (EVar _ name) = HVar (Ident name)
genExpr (EAssign _ name expr) = HAssignExpr (Ident name) (genExpr expr)
genExpr (EUnary _ unaryOp expr) = HUnary unaryOp (genExpr expr)
genExpr (EBinary _ op e1 e2) =
    HBin op (genExpr e1) (genExpr e2)
genExpr (EIf _ c t e) = HIf (genExpr c) (genExpr t) (genExpr e)
genExpr (EBlock _ stmts maybeExpr) =
    let hirStmts = map genStmt stmts
        hirExpr = case maybeExpr of
            Nothing -> HLit HNil
            Just e -> genExpr e
    in HBlock hirStmts hirExpr

genStmt :: Stmt -> HIRStmt
genStmt (SVarDecl _ name expr) = HAssign (Ident name) (genExpr expr)
genStmt (SExpr _ expr) = HExpr (genExpr expr)
genStmt (SReturn _ maybeExpr) = case maybeExpr of
    Nothing -> HReturn (HLit HNil)
    Just expr -> HReturn (genExpr expr)