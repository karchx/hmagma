module HMagma.Environment where

import Data.IORef
import qualified Data.Map.Strict as Map

data RuntimeError
    = UnboudVariable String
    | TypeError String
    | DivisionByZero
    | InvalidArgsCount Int Int
    | GeneralError String
    deriving (Show, Eq)

data Value
    = VNum Double
    | VBool Bool
    | VStr String
    | VNil
    | VClosure [String] [Stmt] Env
    | VPrimitive ([Value] -> IO (Either RuntimeError Value))

instance Show Value where
    show (VNum n)               = show n
    show (VBool b)              = if b then "true" else "false"
    show (VStr s)               = "\"" ++ s ++ "\""
    show VNil                   = "nothing"
    show (VClosure params _ _)  = "<fn (" ++ unwords params ++ ")>"
    show (VPrimitive _)         = "<native fn>"

instance Eq Value where
    (VNum a)    == (VNum b)  = a == b
    (VBool a)   == (VBool b) = a == b
    (VStr a)    == (VStr b)  = a == b
    VNil        == VNil      = True
    _           == _         = False

data Stmt
    = StmtExpr Expr
    | StmtVar String Expr
    | StmtBlock [Stmt]
    | StmtIf Expr Stmt (Maybe Stmt)
    | StmtWhile Expr Stmt
    | StmtFunction String [String] [Stmt]
    | StmtReturn (Maybe Expr)
    deriving (Show, Eq)

data Expr
    = Lit Value
    | Var String
    | Assign String Expr
    | Binary String Expr Expr
    | Unary String Expr
    | Logical String Expr Expr
    | Call Expr [Expr]
    deriving (Show, Eq)

data Env = Env
    { bindings  :: IORef (Map.Map String Value)
    , parent    :: Maybe Env 
    }

nullEnv :: IO Env
nullEnv = do
    ref <- newIORef Map.empty
    pure $ Env { bindings = ref, parent = Nothing }

extendEnv :: Env -> [(String, Value)] -> IO Env
extendEnv parentEnv initialBindings = do
    ref <- newIORef (Map.fromList initialBindings)
    pure $ Env { bindings = ref, parent = Just parentEnv }

lookupVar :: Env -> String -> IO (Either RuntimeError Value)
lookupVar env name = do
    mapCurrent <- readIORef (bindings env)
    case Map.lookup name mapCurrent of
        Just val    -> pure (Right val)
        Nothing     -> case parent env of
            Just pEnv   -> lookupVar pEnv name
            Nothing     -> pure (Left $ UnboudVariable name)


defineVar :: Env -> String -> Value -> IO ()
defineVar env name val = modifyIORef' (bindings env) (Map.insert name val)

assignVar :: Env -> String -> Value -> IO (Either RuntimeError Value)
assignVar env name val = do
    mapCurrent <- readIORef (bindings env)
    if Map.member name mapCurrent
        then do
            modifyIORef' (bindings env) (Map.insert name val)
            pure (Right val)
        else case parent env of
            Just pEnv   -> assignVar pEnv name val
            Nothing     -> pure (Left $ UnboudVariable name)
