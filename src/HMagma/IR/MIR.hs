
module HMagma.IR.MIR
    ( CFG
    , PredMap
    , BasicBlock(..)
    , CFGBuilder(..)
    , CFGBuilderState(..)
    , Terminator(..)
    , Label(..)
    , buildPredMap
    , getSuccessors
    , getBlock
    , getPredecessors
    ) where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import HMagma.IR.HIR

type Label = Int

type PredMap = IM.IntMap [Label]

-- terminator edges of the graph
data Terminator
    = TBranch HIRExpr Label Label
    | TJump Label
    | TReturn HIRExpr
    | TExit
    deriving (Show)

data BasicBlock = BasicBlock
    { bbId :: Label
    , bbStmts :: [HIRStmt] -- lineal instructions
    , bbTerm :: Terminator
    } deriving (Show)

type CFG = IM.IntMap BasicBlock

data CFGBuilderState = CFGBuilderState
    { nextLabel :: Label
    , nextTemp :: Int
    , currentBlock :: Label
    , stmtsBuffer :: [HIRStmt]
    , graph :: CFG
    }

type CFGBuilder a = State CFGBuilderState a

buildPredMap :: CFG -> PredMap
buildPredMap cfg = IM.foldlWithKey' addEdges IM.empty cfg
    where
        addEdges acc currentBlockId block =
            let succs = getSuccessors block
            in foldl' (\m succId -> IM.insertWith (++) succId [currentBlockId] m) acc succs

getSuccessors :: BasicBlock -> [Label]
getSuccessors block = case bbTerm block of
    TReturn _ -> []
    TJump target -> [target]
    TBranch _ trueTarget falseTarget -> [trueTarget, falseTarget]
    TExit -> []

getBlock :: CFG -> Label -> BasicBlock
getBlock cfg node = IM.findWithDefault emptyBlock node cfg
    where
        emptyBlock = BasicBlock node [] (TExit)

getPredecessors :: PredMap -> Label -> [Label]
getPredecessors predMap node = IM.findWithDefault [] node predMap
