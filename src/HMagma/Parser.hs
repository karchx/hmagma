{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module HMagma.Parser 
    ( parseHMagma
    , pProgram
    , pExpr
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import HMagma.AST
import HMagma.Lexer (sc, lexeme, symbol)

type Parser = Parsec Void Text

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

withPos :: (SourcePos -> Parser a) -> Parser a
withPos p = getSourcePos >>= p

pLiteral :: Parser Expr
pLiteral = withPos \pos -> choice
    [ ELiteral pos . LitFloat <$> try (lexeme L.float)
    , ELiteral pos . LitInt <$> lexeme L.decimal
    , ELiteral pos . LitString <$> pStringLiteral
    , ELiteral pos (LitBool True) <$ symbol "true"
    , ELiteral pos (LitBool False) <$ symbol "false"
    , ELiteral pos LitNil <$ symbol "nothing"
    ]
    where
        pStringLiteral = lexeme $ T.pack <$> between (char '"') (char '"') (many pChar)
        pChar = (char '\\' *> pEscape) <|> satisfy (/= '"')
        pEscape = choice
            [ '\n' <$ char 'n', '\t' <$ char 't', '\r' <$ char 'r'
            , '\\' <$ char '\\', '"' <$ char '"'
            ]


pVar :: Parser Expr
pVar = withPos $ \pos -> EVar pos <$> pIdentifier

pIdentifier :: Parser Text
pIdentifier = lexeme $ checkReserved *> (packIdent <$> firstChar <*> restChars)
    where
        checkReserved = notFollowedBy (choice $ map symbol ["if", "then", "else", "let", "fn", "while", "return", "true", "false", "nothing"])
        firstChar = letterChar <|> char '_'
        restChars = many (alphaNumChar <|> char '_' <|> char '?')
        packIdent c cs = T.pack (c : cs)

pSExpr :: Parser Expr
pSExpr = withPos $ \pos -> parens (ESExpr pos <$> pOpName <*> many pExpr)
    where
        pOpName = pIdentifier <|> lexeme (T.pack <$> some pOpChar)
        pOpChar = oneOf ("+-*/%<=>!&|" :: String)

pBlock :: Parser Expr
pBlock = withPos $ \pos -> braces (EBlock pos <$> many (try pStmt) <*> optional pExpr)

pIfExpr :: Parser Expr
pIfExpr = withPos $ \pos -> EIf pos <$> pCond <*> pThen <*> pElse
    where
        pCond = symbol "if" *> (parens pExpr <|> pExpr)
        pThen = optional (symbol "then") *> pExpr
        pElse = symbol "else" *> pExpr

pLetExpr :: Parser Expr
pLetExpr = withPos $ \pos -> ELet pos <$> varName <*> valExpr <*> bodyExpr
    where
        varName = symbol "let" *> (pIdentifier)
        valExpr = symbol "=" *> pExpr
        bodyExpr = symbol "in" *> pExpr

pLambda :: Parser Expr
pLambda = withPos $ \pos -> ELambda pos <$> params <*> body
    where
        params = symbol "fn" *> parens (pIdentifier `sepBy` symbol ",")
        body = (symbol "=>" <|> symbol "->") *> pExpr

pPrimary :: Parser Expr
pPrimary = choice
    [ try pSExpr, try pLambda, try pIfExpr, try pLetExpr, try pBlock, try pLiteral, pVar, parens pExpr]

pApplication :: Parser Expr
pApplication = applyApp <$> getSourcePos <*> pPrimary <*> pArgs
    where
        pArgs = optional (parens (pExpr `sepBy` symbol ","))

        applyApp _ base Nothing         = base
        applyApp pos base (Just args)   = EApp pos base args

pExpr :: Parser Expr
pExpr = makeExprParser pApplication table
    where
        table =
            [ [ Prefix (unaryOp "-" OpNeg), Prefix (unaryOp "!" OpNot) ]
            , [ InfixL (binaryOp "*" OpMul), InfixL (binaryOp "/" OpDiv), InfixL (binaryOp "%" OpMod) ] 
            , [ InfixL (binaryOp "+" OpAdd), InfixL (binaryOp "-" OpSub) ]
            , [ InfixN (binaryOp "<=" OpLte), InfixN (binaryOp ">=" OpGte), 
                InfixN (binaryOp "<" OpLt), InfixN (binaryOp ">" OpGt) ]
            , [ InfixN (binaryOp "==" OpEq), InfixN (binaryOp "!=" OpNeq) ]
            , [ InfixL (binaryOp "&&" OpAnd) ]
            , [ InfixL (binaryOp "||" OpOr) ]
            ]
        unaryOp name op = (\pos -> EUnary pos op) <$> getSourcePos <* symbol name
        binaryOp name op = (\pos -> EBinary pos op) <$> getSourcePos <* symbol name

pStmt :: Parser Stmt
pStmt = choice [ pFunDef, pVarDecl, pWhileStmt, pReturnStmt, pExprStmt ]

pVarDecl :: Parser Stmt
pVarDecl = withPos $ \pos -> SVarDecl pos 
    <$> (symbol "let" *> pIdentifier)
    <*> (symbol "=" *> pExpr <* symbol ";")

pFunDef :: Parser Stmt
pFunDef = withPos $ \pos -> SFuncDef pos
    <$> (symbol "fn" *> pIdentifier)
    <*> (parens (pIdentifier `sepBy` symbol ","))
    <*> (symbol "=" *> pExpr <* optional (symbol ";"))

pWhileStmt :: Parser Stmt
pWhileStmt = withPos $ \pos -> SWhile pos
    <$> (symbol "while" *> parens pExpr)
    <*> pStmt

pReturnStmt :: Parser Stmt
pReturnStmt = withPos $ \pos -> SReturn pos
    <$> (symbol "return" *> optional pExpr <* symbol ";")

pExprStmt :: Parser Stmt
pExprStmt = withPos $ \pos -> SExpr pos
    <$> (pExpr <* symbol ";")

pProgram :: Parser [Stmt]
pProgram = sc *> many pStmt <* eof

parseHMagma :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Stmt]
parseHMagma = runParser pProgram
