{-# LANGUAGE GADTs #-}

module HMagma.OptPasses ( constantFold ) where

import Data.Fixed
import qualified Data.Map as Map
import qualified Data.Set as Set
import HMagma.HIR 
    ( HIRExpr(..)
    , HIRLit(..)
    , HIRStmt(..)
    , HIRProg(..)
    , HIRFun(..)
    )
import HMagma.AST (BinaryOp(..))
import Graph.ControlFlow (CFG(..), BasicBlock(..))

type NodeId = Int
type VarState = Map.Map String (Lattice Integer)
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
evalOpInt OpMult a b = Const (HInt (a * b))
evalOpInt OpSub a b = Const (HInt (a - b))
evalOpInt OpMod _ 0 = Bottom
evalOpInt OpMod a b = Const (HInt (a `mod` b))
evalOpInt OpDiv _  0 = Bottom -- division by zero
evalOpInt OpDiv a b = Const (HInt (a `div` b))

evalOpFloat :: BinaryOp -> Lattice Double -> Lattice Double -> Lattice Double
evalOpFloat OpAdd a b = Const (HFloat (a + b))
evalOpFloat OpMult a b = Const (HFloat (a * b))
evalOpFloat OpSub a b = Const (HFloat (a - b))
evalOpFloat OpDiv _ 0 = Bottom -- division by zero
evalOpFloat OpDiv a b = Const (HFloat (a / b))
evalOpFloat OpMod _ 0 = Bottom
evalOpFloat OpMod a b = Const (HFloat (mod' a b))

evalExpr :: VarState -> HIRExpr -> Lattice HIRLit
evalExpr _ (HIRLit (HInt n)) = Const (HInt n)
evalExpr _ (HIRLit (HFloat n)) = Const (HFloat n)
evalExpr state (HBin op l r) =
    let l' = evalExpr state l
        r' = evalExpr state r
    in case (l', r') of
        (Bottom, _) -> Bottom
        (_, Bottom) -> Bottom
        (Top, _)    -> Top
        (_, Top)    -> Top

        (Const (HInt _), Const (HInt _))        -> evalIntExpr op l' r'
        (Const (HFloat _), Const (HFloat _))    -> evalFloatExpr op l' r'
        _                                       -> Bottom

evalInstr :: VarState -> HIRStmt -> VarState
evalInstr state (HAssign (Ident lhs) rhs) =
    let
        newValue = evalExpr state rhs
        oldValue = Map.findWithDefault Top lhs state
        joinedValue = joinLattice oldValue newValue
    in
        Map.insert lhs joinedValue state
evalInstr state _ = state

transferFunction :: VarState -> BasicBlock -> (VarState, [NodeId])
transferFunction currentState block = foldl evalInstr currentState (bbStmts block)

worklist :: Set.Set NodeId -> VarState -> CFG -> VarState
worklist queue state cfg =
    case Set.minView queue of
         Nothing -> state
         Just (node, restQueue) ->
            let
                (newState, changed) = transferFunction node state cfg

                newQueue = if changed
                           then foldr Set.insert restQueue (successors cfg node)
                           else restQueue
            in
                worklist newQueue newState cfg

constantFold :: HIRProg -> HIRProg
constantFold (HIRProg funcs globals) =
    HIRProg (map foldFun funcs) (map foldStmt globals)

-- constPropagation :: HIRProg -> HIRProg
