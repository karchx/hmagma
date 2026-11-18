{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Bifunctor (first)
import Text.Megaparsec (errorBundlePretty)
import HMagma.Frontend.Parser
import HMagma.Frontend.TypeChecker
import HMagma.IR.HIR
import HMagma.Backend.CudaGen
import HMagma.Transforms.SCCP

compiler :: String -> T.Text -> Either String T.Text
compiler filename src = do
    ast <- first errorBundlePretty $ parseHMagma filename src

    -- Generate HIR
    hirProg <- typeCheck (genProgramHIR ast)
    let optimized = optimizeProgram hirProg

    pure (codegen optimized)

main :: IO ()
main = do
    let inputFile = "examples/gravitySafe.hmag"
        outputFile = "examples/gravitySafe.cu" -- TODO: dynamic filename

    src <- TIO.readFile "examples/gravitySafe.hmag"

    case compiler inputFile src of
        Left err -> do
            putStrLn "Failed compile"
            putStrLn err
        Right code -> do
            TIO.writeFile outputFile code
