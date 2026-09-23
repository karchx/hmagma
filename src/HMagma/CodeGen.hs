{-# LANGUAGE OverloadedStrings #-}

module HMagma.CodeGen ( codegen ) where

import qualified Data.Text as T
import Data.Text (Text)
import HMagma.HIR
import HMagma.AST

codeFun :: HIRFun -> Text
codeFun (HIRFun (Ident name) params body) =
    "__device__ double " <> name <> "(" <>
    T.intercalate "," [ "double " <> p | Ident p <- params ] <>
    ") {\n" <>
    codegenExpr body <>
    "\n}\n"

codegenExpr :: HIRExpr -> Text
codegenExpr (HLit (HInt n))   = T.pack (show n) <> ".0"
codegenExpr (HLit (HFloat f)) = T.pack (show f)
codegenExpr (HVar (Ident x))  = x
codegenExpr (HBin OpAdd a b)  = "(" <> codegenExpr a <> " + " <> codegenExpr b <> ")"
codegenExpr (HBin OpSub a b)  = "(" <> codegenExpr a <> " - " <> codegenExpr b <> ")"
codegenExpr (HBin OpMul a b)  = "(" <> codegenExpr a <> " * " <> codegenExpr b <> ")"
codegenExpr (HBin OpDiv a b)  = "(" <> codegenExpr a <> " / " <> codegenExpr b <> ")"
codegenExpr (HBin OpEq a b)   = "(" <> codegenExpr a <> " == " <> codegenExpr b <> ")"
codegenExpr (HIf c t e)       =
    "(" <> codegenExpr c <> " ? " <>
    codegenExpr t <> " : " <>
    codegenExpr e <> ")"
codegenExpr (HBlock _ exprs) = codegenExpr exprs

codegen :: HIRProg -> Text
codegen (HIRProg funcs _) = T.intercalate "\n\n" (map codeFun funcs)