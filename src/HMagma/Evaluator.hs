{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HMagma.Evaluator where

import HMagma.Environment
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class

newtype EvalM a = EvalM
    { unEvalM :: ReaderT Env (ExceptT RuntimeError IO) a
    } 
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError, RuntimeError, MonadIO)

runEvalM :: Env -> EvalM a -> IO (Either RuntimeError a)
runEvalM env action = runExceptT (runReaderT (unEvalM action) env)

evalExpr :: Expr -> EvalM Value
evalExpr (Lit, val) = pure val
evalExpr (Var name) = ask >>= \env -> liftIO (lookupVar env name) >>= liftEither
evalExpr (Assign name expr) = do
    val <- evalExpr expr
    env <- ask
    liftIO (assignVar env name val) >>= liftEither
evalExpr (Unary op expr) = do
    val <- evalExpr expr
    case (op, val) of
        ("-", VNum n)  -> pure $ VNum (-n)
        ("!", VBool b) -> pure $ (VBool) (not b)
        ("!", VNil)    -> pure $ VBool True
        ("!", _)       -> pure $ VBool False
        '_'            -> throwError $ TypeError ("Unary operator invalid: " ++ op)
