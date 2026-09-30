module Graph.ControlFlow where

import qualified Data.IntMap.Strict as IM
import Control.Monad.State.Strict

import HMagma.HIR (HIRExpr(..), HIRStmt(..))

type Label = Int

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
    , graph :: CFG
    }

type CFGBuilder a = State CFGBuilderState a

freshLabel :: CFGBuilder Label
freshLabel = do
    s <- get
    let l = nextLabel s
    put (s { nextLabel = l + 1 })
    return l

emitBlock :: BasicBlock -> CFGBuilder ()
emitBlock bb = modify $ \s ->
    s { graph = IM.insert (bbId bb) bb (graph s) }


buildCFG :: [HIRStmt] -> Label -> Label -> CFGBuilder ()
buildCFG [] currentLabel exitLabel =
    emitBlock $ BasicBlock currentLabel [] (TJump exitLabel)

buildCFG (stmt:stmts) currentLabel exitLabel = case stmt of
    HAssign _ _ ->
        buildCFG stmts currentLabel exitLabel
    HIf cond thenStmt elseStmt -> do
        trueLabel   <- freshLabel
        falseLabel  <- freshLabel
        mergeLabel  <- freshLabel

        emitBlock $ BasicBlock currentLabel [] (TBranch cond trueLabel falseLabel)

        buildCFG [thenStmt] trueLabel mergeLabel
        buildCFG [elseStmt] falseLabel mergeLabel
        buildCFG stmts mergeLabel exitLabel

    HReturn expr ->
        emitBlock $ BasicBlock currentLabel [] (TReturn expr)
    HBlock innerStmts _ ->
        buildCFG (innerStmts ++ stmts) currentLabel exitLabel

buildFunctionCFG :: HIRFun -> CFG
buildFunctionCFG fun =
    let initialSate = CFGBuilderState { nextLabel = 1, graph = IM.empty }
        entryLabel = 0
        exitLabel = -1
