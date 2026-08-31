{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import HMagma.Parser
import HMagma.IR


main :: IO ()
main = do
    src <- TIO.readFile "example.hmag"
    let stmt = parseHMagma "example.hmag" src
    case stmt of 
        Left err -> print err
        Right res -> print $ genProgramIR res
