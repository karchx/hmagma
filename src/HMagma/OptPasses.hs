module HMagma.OptPasses ( constantFold ) where

import HMagma.HIR 
    ( HIRExpr(..)
    , HIRLit(..)
    , HIRStmt(..)
    , HIRProg(..)
    , HIRFun(..)
    )
import HMagma.AST (BinaryOp(..))

evalInt :: BinaryOp -> Integer -> Integer -> Maybe Integer
evalInt OpAdd a b = Just (a + b)
evalInt OpSub a b = Just (a - b)
evalInt OpMul a b = Just (a * b)
evalInt OpDiv a b
    | b == 0    = Nothing
    | otherwise = Just (a `div` b)
evalInt OpMod a b
    | b == 0    = Nothing
    | otherwise = Just (a `mod` b)
evalInt _ _ _     = Nothing

evalFloat :: BinaryOp -> Double -> Double -> Maybe Double
evalFloat OpAdd a b = Just (a + b)
evalFloat OpSub a b = Just (a - b)
evalFloat OpMul a b = Just (a * b)
evalFloat OpDiv a b
    | b == 0    = Nothing
    | otherwise = Just (a / b)
evalFloat OpMod _ _ = Nothing
evalFloat _ _ _     = Nothing


foldExpr :: HIRExpr -> HIRExpr
foldExpr (HBin op l r) =
    let l' = foldExpr l
        r' = foldExpr r
    in case(l', r') of
        (HLit (HInt a), HLit (HInt b)) ->
            case evalInt op a b of
                Just v -> HLit (HInt v)
                Nothing -> HBin op l' r'
        (HLit (HFloat a), HLit (HFloat b)) ->
            case evalFloat op a b of
                Just v -> HLit (HFloat v)
                Nothing -> HBin op l' r'
        _ -> HBin op l' r'

foldExpr (HIf cond t e) =
    let c' = foldExpr cond
    in case c' of
        HLit (HBool True)   -> foldExpr t
        HLit (HBool False)  -> foldExpr e
        _                   -> HIf c' (foldExpr t) (foldExpr e)

foldExpr (HBlock stmts expr) =
    HBlock (map foldStmt stmts) (foldExpr expr)
foldExpr e = e

foldStmt :: HIRStmt -> HIRStmt 
foldStmt (HAssign a expr) = HAssign a (foldExpr expr)
foldStmt (HExpr expr) = HExpr (foldExpr expr)
foldStmt (HReturn rexpr) = HReturn (foldExpr rexpr)

foldFun :: HIRFun -> HIRFun
foldFun (HIRFun name params body) = newFun
    where
        newFun = HIRFun
            { funName = name
            , funParams = params
            , funBody = foldExpr body 
            }

constantFold :: HIRProg -> HIRProg
constantFold (HIRProg funcs globals) =
    HIRProg (map foldFun funcs) (map foldStmt globals)