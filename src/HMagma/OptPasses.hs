module HMagma.OptPasses where

import HMagma.AST

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

-- type VNTable = Map (BinaryOp, Operand, Operand) Temp
-- type VNTableRename = Map Temp Temp
-- 
-- 
-- rewriteOp :: VNTableRename -> Operand -> Operand
-- rewriteOp r (OTemp t) = OTemp (M.findWithDefault t t r)
-- rewriteOp _ o         = o

-- localValueNumbering :: [TAC] -> [TAC]
-- localValueNumbering = go M.empty M.empty
--     where
--         go :: VNTable -> VNTableRename -> [TAC] -> [TAC]
--         go _ _ [] = []
--         go vn rename (i:is) = case i of
--             TBinOp dest op a b ->
--                 let key = (op, a, b)
--                 in case M.lookup key vn of
--                     Just old ->
--                         -- ignore redundancy code
--                         -- and save register for rename redundancy
--                         let rename' = M.insert dest old rename
--                         in go vn rename' is
--                     Nothing ->
--                         let vn' = M.insert key dest vn
--                         in TBinOp dest op a b : go vn' rename is
--             TAssign dest src ->
--                 let src' = rewriteOp rename src
--                 in TAssign dest src' : go vn rename is
-- 