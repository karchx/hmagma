{-# LANGUAGE DeriveFunctor #-}

module HMagma.Graph where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (nub, foldl')

type Vertex = Int -- basic block
type Graph = Map Vertex [Vertex] -- adj (succesor)

empty :: Graph
empty = Map.empty

insertVertex :: Vertex -> Graph -> Graph
insertVertex v g = Map.insertWith (++) v [] g

-- u -> v
insertEdge :: Vertex -> Vertex -> Graph -> Graph
insertEdge u v g =
    let g' = insertVertex (insertVertex v g)
    in Map.adjust (nub . (v:)) u g'

-- predecessors
predecessors :: Graph -> Vertex -> [Vertex]
predecessors g v =
    [u | (u, succs) <- Map.toList g, v `elem` succs]

successors :: Graph -> Vertex -> [Vertex]
successors g v = fromMaybe [] (Map.lookup v g)

-- DFS
dfs :: Grap -> Vertex -> [Vertex]
dfs g start = go [start] []
    where
        go [] visited = reverse visited
        go (x:xs) visited
            | x `elem` visited = go xs visited
            | otherwise =
                let succs = successors g x
                in go (succs ++ xs) (x:visited)

-- TESTING IR
data Instr
    = Label String
    | Assign String String
    | BinOp String String String String
    | Jump String
    | CJump String String String
    | Return
    deriving (Show, Eq)

exampleIR :: [Instr]
exampleIR =
    [ Label "entry"
    , Assign "n" "5"
    , Assign "acc" "1"
    , Label "loop"
    , CJump "n" "body" "exit"
    , Label "body"
    , BinOp "acc" "acc" "*" "n"
    , Jump "loop"
    , Label "exit"
    , Return
    ]

leaders :: [Instr] -> [Int]
leaders instrs =
    let n = length instrs
        isLeader i =
            i == 0 ||
            case instrs !! i of
                Label _ -> True
                _       -> False
            ||
            (i > 0 && case instrs !! (i-1) of
                        Jump _      -> True
                        CJump _ _ _ -> True
                        _           -> False)
    in filter isLeader [0..n-1]


type Block = (Vertex, [Instr])

buildBlocks :: [Instr] -> [Block]
buildBlocks instrs =
    let ls = leaders instrs
        ends = map (\i -> fromMaybe (length instrs) (lookup (i+1) (zip [0..] ls))) [0..]
        ranges = zip ls (tail ls ++ [length instrs])
    in zipWith (\i (start,end) -> (i, take (end-start) (drop start instrs)))
                [0..] ranges

buildCFG :: [Instr] -> (Grap, [Block])
buildCFG instrs =
    let blocks = buildBlocks instrs
        labelMap = Map.fromList
            [(l, vid) | (vid, (Label l : _)) <- blocks]
        addEdges g (vid, stmts) =
            case reverse stmts of
                (Jump t : _) ->
                    case Map.lookup t labelMap of
                        Just tgt -> insertEdge vid tgt g
                        Nothing  -> g
                (CJump _ t f : _) ->
                    let g1 = maybe g (\tgt -> insertEdge vid tgt g) (Map.lookup t labelMap)
                    in maybe g1 (\tgt -> insertEdge vid tgt g1) (Map.lookup f labelMap)
                (Return : _) -> g
                _ ->
                    let next = vid + 1
                    in if next < length blocks then insertEdge vid next g else g
    in (foldl' addEdges empty blocks, blocks)
