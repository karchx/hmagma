{-# LANGUAGE OverloadedStrings #-}

module HMagma.Lexer 
    ( sc
    , lexeme
    , symbol
    , tokenLexer
    , tokenize
    ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as L

import HMagma.Tokens (TokenType(..))
import qualified HMagma.Tokens as HT

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keywords :: [Text]
keywords = ["if", "then", "else", "let", "fn", "while", "return", "true", "false",
    "nothing", "and", "or", "not"]

keywordSet :: Set.Set Text
keywordSet = Set.fromList keywords

pNumber :: Parser TokenType
pNumber = lexeme $ try (TokFloat <$> L.float) <|> (TokInt <$> L.decimal)

pString :: Parser TokenType
pString = lexeme $ TokString . T.pack <$> between (char '"') (char '"') (many pChar)
    where
        pChar = (char '\\' *> pEscape) <|> satisfy (/= '"')
        pEscape = choice
            [ '\n' <$ char 'n', '\t' <$ char 't', '\r' <$ char 'r'
            , '\\' <$ char '\\', '"' <$ char '"'
            ]

pIdentOrKeyword :: Parser TokenType
pIdentOrKeyword = lexeme $ classify . T.pack <$> ((:) <$> startChar <*> many bodyChar)
    where
        startChar = letterChar <|> char '_'
        bodyChar = alphaNumChar <|> char '_' <|> char '?'

        classify "true" = TokBool True
        classify "false" = TokBool False
        classify "nothing" = TokNil
        classify w
            | Set.member w keywordSet = TokKeyword w
            | otherwise               = TokIdent w

pOperator :: Parser TokenType
pOperator = lexeme $ classify <$> op
    where
        op = choice
            [ string "==", string "!=", string "<=", string ">="
            , string "&&", string "||", string "->", string "=>"
            , string "+" , string "-" , string "*",  string "/" 
            , string "%",  string "<",  string ">",  string "!"
            ]

        classify "->" = TokArrow
        classify "=>" = TokFatArrow
        classify o    = TokOp o

pSingleToken :: Parser HT.Token
pSingleToken = (\pos (txt, tt) -> HT.Token tt pos txt)
    <$> getSourcePos
    <*> match (choice
            [ TokLParen    <$ symbol "("
            , TokRParen    <$ symbol ")"
            , TokLBrace    <$ symbol "{"
            , TokRBrace    <$ symbol "}"
            , TokLBracket  <$ symbol "["
            , TokRBracket  <$ symbol "]"
            , TokComma     <$ symbol ","
            , TokSemicolon <$ symbol ";"
            , TokColon     <$ symbol ","
            , TokAssign    <$ symbol "="
            , pOperator, pNumber, pString, pIdentOrKeyword
            ])

tokenLexer :: Parser [HT.Token]
tokenLexer = sc *> many pSingleToken <* eof

tokenize :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [HT.Token]
tokenize = runParser tokenLexer