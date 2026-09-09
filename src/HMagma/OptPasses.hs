module HMagma.OptPasses where

import HMagma.IR (Operand(..), TAC(..), Temp(..))
import HMagma.AST

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

type VNTable = Map (BinaryOp, Operand, Operand) Temp

vnRename :: Map Temp Temp
vnRename = M.empty

localValueNumbering :: [TAC] -> [TAC]
localValueNumbering = go M.empty
    where
        go :: VNTable -> [TAC] -> [TAC]
        go _ [] = []
        go vn (i:is) = case i of
            TBinOp dest op a b ->
                let key = (op, a, b)
                in case M.lookup key vn of
                    Just old ->
                        -- ignore redundancy code
                        -- and save register for rename redundancy
                        let _ = M.insert old (Temp 2) vnRename
                        in go vn is
                    Nothing ->
                        let vn' = M.insert key dest vn
                        in TBinOp dest op a b : go vn' is
            TAssign dest src ->
                TAssign dest src : go vn is
