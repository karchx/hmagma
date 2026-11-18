{-# LANGUAGE GADTs #-}

module HMagma.Transforms.SCCP 
    ( optimizeProgram
    , OptimizedFun(..)
    ) where

import Data.Fixed
import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as Set
import Data.Text (Text)
import HMagma.IR.HIR 
    ( HIRExpr(..)
    , HIRLit(..)
    , HIRStmt(..)
    , HIRProg(..)
    , HIRFun(..)
    , Ident(..)
    )
import HMagma.IR.MIR
    ( CFG
    , PredMap
    , BasicBlock(..)
    , Terminator(..)
    , buildPredMap
    , getBlock
    , getPredecessors
    )
import HMagma.IR.Lowering ( buildFunctionCFG )
import HMagma.Frontend.AST (BinaryOp(..))

data OptimizedFun = OptimizedFun Ident [Ident] CFG

type NodeId = Int
type ParamList = [Ident]
type VarState = Map.Map Text (Lattice HIRLit)
type OutState = Map.Map NodeId (VarState)
data Lattice a = Top | Const a | Bottom deriving (Eq, Show)


-- Params functions for default is Bottom
initEntryState :: ParamList -> VarState
initEntryState params = Map.fromList [ (name, Bottom) | Ident name <- params ]

joinLattice :: Eq a => Lattice a -> Lattice a -> Lattice a
joinLattice Top x = x
joinLattice x Top = x
joinLattice Bottom _ = Bottom
joinLattice _ Bottom = Bottom
joinLattice (Const a) (Const b)
    | a == b    = Const a
    | otherwise =  Bottom

fromBool :: Bool -> Lattice HIRLit
fromBool True = Const (HInt 1)
fromBool False = Const (HInt 0)

evalOpInt :: BinaryOp -> Integer -> Integer -> Lattice HIRLit
evalOpInt OpAdd a b  = Const (HInt (a + b))
evalOpInt OpMul a b  = Const (HInt (a * b))
evalOpInt OpSub a b  = Const (HInt (a - b))
evalOpInt OpMod _ 0  = Bottom
evalOpInt OpMod a b  = Const (HInt (a `mod` b))
evalOpInt OpDiv _  0 = Bottom -- division by zero
evalOpInt OpDiv a b  = Const (HInt (a `div` b))
evalOpInt OpEq a b   = fromBool (a == b)
evalOpInt OpNeq a b  = fromBool (a /= b)
evalOpInt OpLt a b   = fromBool (a < b)
evalOpInt OpGt a b   = fromBool (a > b)
evalOpInt _ _ _      = Bottom

evalOpFloat :: BinaryOp -> Double -> Double -> Lattice HIRLit
evalOpFloat OpAdd a b = Const (HFloat (a + b))
evalOpFloat OpMul a b = Const (HFloat (a * b))
evalOpFloat OpSub a b = Const (HFloat (a - b))
evalOpFloat OpDiv _ 0 = Bottom -- division by zero
evalOpFloat OpDiv a b = Const (HFloat (a / b))
evalOpFloat OpMod _ 0 = Bottom
evalOpFloat OpMod a b = Const (HFloat (mod' a b))
evalOpFloat OpEq a b  = fromBool (a == b)
evalOpFloat OpNeq a b = fromBool (a /= b)
evalOpFloat OpLt a b  = fromBool (a < b)
evalOpFloat OpGt a b  = fromBool (a > b)
evalOpFloat _ _ _     = Bottom

evalExpr :: VarState -> HIRExpr -> Lattice HIRLit
evalExpr _ (HLit lit) = Const lit
evalExpr state (HVar (Ident name)) = Map.findWithDefault Top name state
evalExpr state (HBin op l r) =
    let l' = evalExpr state l
        r' = evalExpr state r
    in case (l', r') of
        (Bottom, _) -> Bottom
        (_, Bottom) -> Bottom
        (Top, _)    -> Top
        (_, Top)    -> Top

        (Const (HInt a), Const (HInt b))        -> evalOpInt op a b
        (Const (HFloat a), Const (HFloat b))    -> evalOpFloat op a b
        _                                       -> Bottom

evalInstr :: VarState -> HIRStmt -> VarState
evalInstr state (HAssign (Ident lhs) rhs) = Map.insert lhs (evalExpr state rhs) state
evalInstr state _ = state

transferFunction :: VarState -> BasicBlock -> (VarState, [NodeId])
transferFunction inState block =
    let
        finalState = foldl evalInstr inState (bbStmts block)
    in
        case bbTerm block of
            TReturn _ -> (finalState, [])
            TJump target -> (finalState, [target])
            TBranch (HVar (Ident condName)) trueTarget falseTarget ->
                let valueCond = Map.findWithDefault Top condName finalState
                in case valueCond of
                    Const (HInt 1) -> (finalState, [trueTarget])
                    Const (HInt 0) -> (finalState, [falseTarget])
                    Top            -> (finalState, [])
                    _              -> (finalState, [trueTarget, falseTarget])

joinStates :: VarState -> VarState -> VarState
joinStates = Map.unionWith joinLattice

-- function: I(b, OUT)
computeInState :: NodeId -> VarState -> PredMap -> OutState -> VarState
computeInState node entryState predMap outStates =
    let preds = getPredecessors predMap node
        baseState = if node == 0 then entryState else Map.empty
    in foldl (\acc p -> joinStates acc (Map.findWithDefault Map.empty p outStates)) baseState preds

worklist :: VarState -> PredMap -> Set.Set NodeId -> OutState -> CFG -> OutState
worklist entryState predMap queue outStates cfg =
    case Set.minView queue of
         Nothing -> outStates
         Just (node, restQueue) ->
            let block = getBlock cfg node
                inState = computeInState node entryState predMap outStates
                (newState, reachableSuccessors) = transferFunction inState block

                oldState = Map.findWithDefault Map.empty node outStates
            in
                if newState == oldState
                then
                    worklist entryState predMap restQueue outStates cfg
                else
                    let newOutStates = Map.insert node newState outStates
                        newQueue = foldr Set.insert restQueue reachableSuccessors
                    in worklist entryState predMap newQueue newOutStates cfg


runWorkList :: ParamList -> CFG -> OutState
runWorkList params cfg =
    let predMap = buildPredMap cfg
        entryState = initEntryState params
        initQueue = Set.singleton 0
    in worklist entryState predMap initQueue Map.empty cfg

optimizeProgram :: HIRProg -> [OptimizedFun]
optimizeProgram (HIRProg funcs _) = map optimizeFun funcs

optimizeFun :: HIRFun -> OptimizedFun
optimizeFun fun@(HIRFun name params _) =
    let cfg = buildFunctionCFG fun
        outStates = runWorkList params cfg
        optCfg = optimizedCFG outStates cfg
    in OptimizedFun name params optCfg

optimizedCFG :: OutState -> CFG -> CFG
optimizedCFG outStates cfg =
    IM.mapWithKey (\nodeId block -> foldBlock (outStates Map.! nodeId) block) cfg

foldBlock :: VarState -> BasicBlock -> BasicBlock
foldBlock state block =
    block { bbStmts = map (foldStmt state) (bbStmts block)
          , bbTerm = foldTerm state (bbTerm block) }

foldStmt :: VarState -> HIRStmt -> HIRStmt
foldStmt state (HAssign idStmt rhs) = HAssign idStmt (foldExpr state rhs)
foldStmt state (HExpr expr) = HExpr (foldExpr state expr)
foldStmt _ stmt = stmt

foldExpr :: VarState -> HIRExpr -> HIRExpr
foldExpr state expr@(HVar (Ident name)) =
    case Map.findWithDefault Top name state of
        Const lit -> HLit lit
        _         -> expr
foldExpr state (HBin op l r) =
    let l' = foldExpr state l
        r' = foldExpr state r
    in case (l', r') of
        (HLit (HInt a), HLit (HInt b)) ->
            case evalOpInt op a b of
                Const lit -> HLit lit
                _         -> HBin op l' r'
        (HLit (HFloat a), HLit (HFloat b)) ->
            case evalOpFloat op a b of
                Const lit -> HLit lit
                _         -> HBin op l' r'
        _ -> HBin op l' r'
foldExpr _ expr = expr

foldTerm :: VarState -> Terminator -> Terminator
foldTerm state term@(TBranch cond t f) =
    case foldExpr state cond of
        HLit (HInt 1) -> TJump t
        HLit (HInt 0) -> TJump f
        _             -> term
foldTerm state (TReturn expr) = TReturn (foldExpr state expr)
foldTerm _ term = term