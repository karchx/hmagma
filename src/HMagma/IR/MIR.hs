
module HMagma.IR.MIR
    ( CFG
    , PredMap
    , BasicBlock(..)
    , CFGBuilder(..)
    , CFGBuilderState(..)
    , Terminator(..)
    , Label(..)
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