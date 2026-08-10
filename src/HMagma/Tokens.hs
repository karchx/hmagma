{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module HMagma.Tokens
    ( TokenType(..)
    , Token(..)
    , Position 
    ) where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)
import GHC.Generics (Generic)

type Position = SourcePos

data TokenType =
    -- Primitives
      TokInt Integer
    | TokFloat Double
    | TokString Text
    | TokBool Bool
    | TokNil
    -- Keywords
    | TokIdent Text
    | TokKeyword Text

    -- Operators
    | TokOp Text
    | TokLParen | TokRParen
    | TokLBrace | TokRBrace
    | TokLBracket | TokRBracket
    | TokComma | TokSemicolon
    | TokColon | TokAssign
    | TokArrow | TokFatArrow
    | TokEOF
    deriving (Show, Eq, Generic)

data Token = Token
    { tokenType :: !TokenType
    , tokenPos :: !SourcePos
    , tokenText :: !Text
    } deriving (Show, Eq, Generic)
