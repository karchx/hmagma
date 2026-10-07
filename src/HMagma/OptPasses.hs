{-# LANGUAGE GADTs #-}

module HMagma.OptPasses (worklist) where

import Data.Fixed
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import HMagma.HIR 
    ( HIRExpr(..)
    , HIRLit(..)
    , HIRStmt(..)
    , HIRProg(..)
    , HIRFun(..)
    , Ident(..)
    )
import HMagma.AST (BinaryOp(..))
import Graph.ControlFlow (CFG, PredMap, BasicBlock(..), Terminator(..), getBlock, getPredecessors)

type NodeId = Int
type VarState = Map.Map Text (Lattice HIRLit)
type OutState = Map.Map NodeId (VarState)
data Lattice a = Top | Const a | Bottom deriving (Eq, Show)

joinLattice :: Eq a => Lattice a -> Lattice a -> Lattice a
joinLattice Top x = x
joinLattice x Top = x
joinLattice Bottom _ = Bottom
joinLattice _ Bottom = Bottom
joinLattice (Const a) (Const b)
    | a == b    = Const a
    | otherwise =  Bottom

evalOpInt :: BinaryOp -> Integer -> Integer -> Lattice HIRLit
evalOpInt OpAdd a b = Const (HInt (a + b))
evalOpInt OpMul a b = Const (HInt (a * b))
evalOpInt OpSub a b = Const (HInt (a - b))
evalOpInt OpMod _ 0 = Bottom
evalOpInt OpMod a b = Const (HInt (a `mod` b))
evalOpInt OpDiv _  0 = Bottom -- division by zero
evalOpInt OpDiv a b = Const (HInt (a `div` b))

evalOpFloat :: BinaryOp -> Double -> Double -> Lattice HIRLit
evalOpFloat OpAdd a b = Const (HFloat (a + b))
evalOpFloat OpMul a b = Const (HFloat (a * b))
evalOpFloat OpSub a b = Const (HFloat (a - b))
evalOpFloat OpDiv _ 0 = Bottom -- division by zero
evalOpFloat OpDiv a b = Const (HFloat (a / b))
evalOpFloat OpMod _ 0 = Bottom
evalOpFloat OpMod a b = Const (HFloat (mod' a b))

evalExpr :: VarState -> HIRExpr -> Lattice HIRLit
evalExpr _ (HLit lit) = Const lit
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

worklist :: PredMap -> Set.Set NodeId -> OutState -> CFG -> OutState
worklist predMap queue outStates cfg =
    case Set.minView queue of
         Nothing -> outStates
         Just (node, restQueue) ->
            let
                block = getBlock cfg node
                preds = getPredecessors predMap node

                inState = foldl (\acc p -> joinStates acc (Map.findWithDefault Map.empty p outStates)) Map.empty preds

                (newState, reachableSuccessors) = transferFunction inState block

                oldState = Map.findWithDefault Map.empty node outStates
            in
                if newState == oldState
                then
                    worklist predMap restQueue outStates cfg
                else
                    let newOutStates = Map.insert node newState outStates
                        newQueue = foldr Set.insert restQueue reachableSuccessors
                    in worklist predMap newQueue newOutStates cfg

-- constantFold :: HIRProg -> HIRProg
-- constantFold (HIRProg funcs globals) =
--     HIRProg (map foldFun funcs) (map foldStmt globals)

-- constPropagation :: HIRProg -> HIRProg
