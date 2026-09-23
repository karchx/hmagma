module HMagma.TypeChecker ( typeCheck ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import HMagma.HIR
import HMagma.AST

data Type 
    = TDouble
    | TBool 
    | TString
    | TNil
    | TUnit 
    | TFun [Type] Type
    deriving (Show, Eq)

type Env = Map.Map Ident Type

type TypeCheck = Either String

isNumeric :: Type -> Bool
isNumeric TDouble = True
isNumeric _       = False

requireNumeric :: Type -> Type -> TypeCheck ()
requireNumeric t1 t2
    | isNumeric t1 && isNumeric t2 = pure ()
    | otherwise = Left $ "Type error: should two Numeric type for " 
                       ++ show t1 ++ " and " ++ show t2

requireSame :: Type -> Type -> TypeCheck ()
requireSame t1 t2
    | t1 == t2    = pure ()
    | otherwise   = Left $ "Type error: expected matching types, but got " 
                         ++ show t1 ++ " and " ++ show t2

checkStmt :: Env -> HIRStmt -> TypeCheck Env
checkStmt env (HAssign x e) = do
    t <- infer env e
    pure $ Map.insert x t env
checkStmt env (HExpr e) = do
    _ <- infer env e
    pure env
checkStmt env (HReturn e) = do
    _ <- infer env e
    pure env

checkFun :: Env -> HIRFun -> TypeCheck ()
checkFun env (HIRFun _ params body) = do
    let env' = foldl (\e p -> Map.insert p TDouble e) env params
    _ <- infer env' body
    pure ()

infer :: Env -> HIRExpr -> TypeCheck Type
infer _ (HLit (HInt _)) = pure TDouble
infer _ (HLit (HFloat _)) = pure TDouble
infer _ (HLit (HBool _)) = pure TBool
infer _ (HLit (HString _)) = pure TString
infer _ (HLit (HNil)) = pure TNil
infer env (HVar x) = case Map.lookup x env of
    Just t -> pure t
    Nothing -> Left $ "Unbound variable: " ++ show x
infer env (HBin op e1 e2) = do
    t1 <- infer env e1
    t2 <- infer env e2
    case () of
        _ | op `elem` [OpAdd, OpSub, OpMul, OpDiv] -> requireNumeric t1 t2 >> pure TDouble
          | op `elem` [OpEq, OpNeq, OpLt, OpLte, OpGt, OpGte] -> requireSame t1 t2 >> pure TBool
          | otherwise -> Left $ "Unbound bin op: " ++ show op
infer env (HIf c t e)   = do
    tc <- infer env c
    requireSame tc TBool
    tt <- infer env t
    te <- infer env e
    requireSame tt te
    pure tt
infer env (HBlock stmts e) = do
    env' <- foldM checkStmt env stmts
    infer env' e
infer env (HAssignExpr _ e) = do
    t <- infer env e
    pure t
infer env (HUnary op e) = do
    t <- infer env e
    case op of
        OpNeg | isNumeric t -> pure TDouble
        OpNot | t == TBool -> pure TBool
        _ -> Left $ "Type error: unary " ++ show op ++ " on " ++ show t


typeCheck :: HIRProg -> TypeCheck HIRProg
typeCheck (HIRProg funcs globals) = do
    env0 <- foldM checkStmt Map.empty globals

    mapM_ (checkFun env0) funcs

    pure (HIRProg funcs globals)
