{-# LANGUAGE OverloadedStrings #-}

module HMagma.CodeGen ( codegen ) where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import HMagma.HIR
import HMagma.AST
import Graph.ControlFlow


codegen :: [OptimizedFun] -> Text
codegen funcs = T.intercalate "\n\n" (map codeFun funcs)

codeFun :: OptimizedFun -> Text
codeFun (OptimizedFun (Ident name) params cfg) =
    let blocks = IM.elems cfg
        assignedVars = Set.fromList [ lhs | BasicBlock _ stmts _ <- blocks, HAssign (Ident lhs) _ <- stmts ]
        localVars = assignedVars `Set.difference` Set.fromList [ p | Ident p <- params ]
        decls = T.concat [ "\tdouble " <> v <> ";\n" | v <- Set.toList localVars ]
    in
    "__device__ double " <> name <> "(" <>
    T.intercalate ", " [ "double " <> p | Ident p <- params ] <>
    ") {\n" <>
    decls <>
    T.concat (map codegenBlock blocks) <>
    "}\n"

codegenBlock :: BasicBlock -> Text
codegenBlock (BasicBlock bId stmts term) =
    "label_" <> T.pack (show bId) <> ":\n" <>
    T.concat (map codegenStmt stmts) <>
    codegenTerm term

codegenStmt :: HIRStmt -> Text
codegenStmt (HAssign (Ident lhs) rhs) =
    "\t" <> lhs <> " = " <> codegenExpr rhs <> ";\n" 
codegenStmt (HExpr expr) =
    "\t" <> codegenExpr expr <> ";\n"
codegenStmt (HReturn expr) =
    "\treturn " <> codegenExpr expr <> ";\n"

codegenTerm :: Terminator -> Text
codegenTerm (TJump target) =
    "\tgoto label_" <> T.pack (show target) <> ";\n"
codegenTerm (TBranch cond trueTarget falseTarget) =
    "\tif (" <> codegenExpr cond <> ") {\n" <>
    "\t\tgoto label_" <> T.pack (show trueTarget) <> ";\n" <>
    "\t} else {\n" <>
    "\t\tgoto label_" <> T.pack (show falseTarget) <> ";\n" <>
    "\t}\n"
codegenTerm (TReturn expr) =
    "\treturn " <> codegenExpr expr <> ";\n"
codegenTerm TExit =
    "\t// exit block\n"

codegenExpr :: HIRExpr -> Text
codegenExpr (HLit (HInt n)) = T.pack (show n) <> ".0"
codegenExpr (HLit (HFloat f)) = T.pack (show f)
codegenExpr (HLit (HNil)) = "NULL"
codegenExpr (HVar (Ident x)) = x
codegenExpr (HBin OpAdd a b) = "(" <> codegenExpr a <> " + " <> codegenExpr b <> ")"
codegenExpr (HBin OpSub a b) = "(" <> codegenExpr a <> " - " <> codegenExpr b <> ")"
codegenExpr (HBin OpMul a b) = "(" <> codegenExpr a <> " * " <> codegenExpr b <> ")"
codegenExpr (HBin OpDiv a b) = "(" <> codegenExpr a <> " / " <> codegenExpr b <> ")"
codegenExpr (HBin OpEq a b) = "(" <> codegenExpr a <> " == " <> codegenExpr b <> ")"
codegenExpr (HBin OpNeq a b) = "(" <> codegenExpr a <> " != " <> codegenExpr b <> ")"
codegenExpr (HBin OpLt a b) = "(" <> codegenExpr a <> " < " <> codegenExpr b <> ")"
codegenExpr (HBin OpGt a b) = "(" <> codegenExpr a <> " > " <> codegenExpr b <> ")"
codegenExpr _ = error "Invalid representation HIRExpr"
