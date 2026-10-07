module Graph.ControlFlow 
    ( buildCFG
    , CFG
    , PredMap
    , BasicBlock(..)
    , Terminator(..)
    , getBlock
    , buildPredMap
    , getPredecessors
    ) where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Control.Monad.State.Strict

import HMagma.HIR (HIRExpr(..), HIRStmt(..), HIRFun(..), HIRProg(..), Ident(..))

type PredMap = IM.IntMap [Label]
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
    , nextTemp :: Int
    , currentBlock :: Label
    , stmtsBuffer :: [HIRStmt]
    , graph :: CFG
    }

type CFGBuilder a = State CFGBuilderState a

freshLabel :: CFGBuilder Label
freshLabel = do
    s <- get
    let l = nextLabel s
    put (s { nextLabel = l + 1 })
    return l

freshTemp :: CFGBuilder Ident
freshTemp = do
    s <- get
    let t = nextTemp s
    put (s { nextTemp = t + 1 })
    return $ Ident (T.pack ("t_" ++ show t))

emitStmt :: HIRStmt -> CFGBuilder ()
emitStmt stmt = modify $ \s ->
    s { stmtsBuffer = stmtsBuffer s ++ [stmt] }

finishBlock :: Terminator -> CFGBuilder ()
finishBlock term = do
    s <- get
    let bb = BasicBlock (currentBlock s) (stmtsBuffer s) term
    put $ s { graph = IM.insert (currentBlock s) bb (graph s) 
            , stmtsBuffer = [] }

startBlock :: Label -> CFGBuilder ()
startBlock label = modify $ \s -> s { currentBlock = label }

lowerExpr :: HIRExpr -> CFGBuilder Ident
lowerExpr expr = case expr of
    HVar ident -> return ident
    HLit lit -> do
        t <- freshTemp
        emitStmt $ HAssign t (HLit lit)
        return t
    HBin op e1 e2 -> do
        t1 <- lowerExpr e1
        t2 <- lowerExpr e2
        tRes <- freshTemp
        emitStmt $ HAssign tRes (HBin op (HVar t1) (HVar t2))
        return tRes

    HIf cond eThen eElse -> do
        condTemp <- lowerExpr cond

        resTemp <- freshTemp

        trueLabel <- freshLabel
        falseLabel <- freshLabel
        mergeLabel <- freshLabel

        finishBlock $ TBranch (HVar condTemp) trueLabel falseLabel

        startBlock trueLabel
        thenTemp <- lowerExpr eThen
        emitStmt $ HAssign resTemp (HVar thenTemp)
        finishBlock $ TJump mergeLabel

        -- False Block
        startBlock falseLabel
        elseTemp <- lowerExpr eElse
        emitStmt $ HAssign resTemp (HVar elseTemp)
        finishBlock $ TJump mergeLabel

        startBlock mergeLabel
        return resTemp
    
    HBlock innerStmts innerExpr -> do
        mapM_ lowerStmt innerStmts
        lowerExpr innerExpr

lowerStmt :: HIRStmt -> CFGBuilder ()
lowerStmt stmt = case stmt of
    HAssign ident expr -> do
        t <- lowerExpr expr
        emitStmt $ HAssign ident (HVar t)

    HExpr expr -> do
        _ <- lowerExpr expr
        return ()

    HReturn expr -> do
        t <- lowerExpr expr
        finishBlock $ TReturn (HVar t)

        deadLabel <- freshLabel
        startBlock deadLabel

buildFunctionCFG :: HIRFun -> CFG
buildFunctionCFG (HIRFun (Ident name) _ body) =
    let entryLabel = 0
        initialState = CFGBuilderState
            { nextLabel = 1
            , nextTemp = 0
            , currentBlock = entryLabel
            , stmtsBuffer = []
            , graph = IM.empty
            }
        (HBlock stmts retExpr) = body

        build = do
            mapM_ lowerStmt stmts

            tRet <- lowerExpr retExpr

            finishBlock $ TReturn (HVar tRet)
        (_, finalState) = runState build initialState
    in graph finalState

buildCFG :: HIRProg -> M.Map Ident CFG
buildCFG (HIRProg funcs _) = M.fromList [ (funName f, buildFunctionCFG f) | f <- funcs ]

buildPredMap :: CFG -> PredMap
buildPredMap cfg = IM.foldlWithKey' addEdges IM.empty cfg
    where
        addEdges acc currentBlockId block =
            let succs = getSuccessors block
            in foldl (\m succId -> IM.insertWith (++) succId [currentBlockId] m) acc succs

getSuccessors :: BasicBlock -> [Label]
getSuccessors block = case bbTerm block of
    TReturn _ -> []
    TJump target -> [target]
    TBranch _ trueTarget falseTarget -> [trueTarget, falseTarget]

getBlock :: CFG -> Label -> BasicBlock
getBlock cfg node = IM.findWithDefault emptyBlock node cfg
    where
        emptyBlock = BasicBlock node [] (TExit)

getPredecessors :: PredMap -> Label -> [Label]
getPredecessors predMap node = IM.findWithDefault [] node predMap
