{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty)
import HMagma.Parser
import HMagma.HIR
import HMagma.TypeChecker


main :: IO ()
main = do
    src <- TIO.readFile "examples/gravitySafe.hmag"
    either print print $ do
        ast <- case parseHMagma "gravitySafe.hmag" src of
                    Left err -> Left (errorBundlePretty err)
                    Right a  -> Right a
        typeCheck (genProgramHIR ast)
