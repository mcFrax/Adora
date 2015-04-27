{-# OPTIONS -XMultiParamTypeClasses #-}
{-# OPTIONS -XFlexibleInstances #-}

module ErrorStateReader where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class

-- To samo mozna by dostac uzywajac ErrorT, StateT i Reader,
-- ale ilosc liftow bylaby przytlaczajaca - zaimplementowanie
-- tego w jednym kawalku eliminuje je wszystkie.

newtype ESR errCls sttCls envCls a = ESR {
    runESR :: sttCls -> envCls -> Either errCls (sttCls, envCls, a)
}

type ErrorStateReader = ESR

instance Monad (ESR errCls sttCls envCls) where
    esr1 >>= esr2 = ESR $ \st env -> do
        (st', env', res1) <- runESR esr1 st env
        runESR (esr2 res1) st' env'
    return x = ESR $ \env st -> return (env, st, x)

instance (Error errCls) => MonadError errCls (ESR errCls sttCls envCls) where
    throwError e = ESR $ \_ _ -> throwError e
    (ESR try) `catchError` h = ESR $ \st e -> do
        (try st e) `catchError` (\err -> runESR (h err) st e)

instance MonadState sttCls (ESR errCls sttCls envCls) where
    get = ESR $ \st env -> return (st, env, st)
    put st = ESR $ \_ env -> return (st, env, ())

instance MonadReader envCls (ESR errCls sttCls envCls) where
    ask = ESR $ \st env -> return (st, env, env)
    local f (ESR run) = ESR $ \st env -> do
        (st', _, res) <- run st (f env)
        return (st', env, res)
