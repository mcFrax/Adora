{-# OPTIONS -XMultiParamTypeClasses #-}
{-# OPTIONS -XFlexibleInstances #-}

module Semantic where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class

import qualified Data.Map as M

import Absadora

import Memory
import Types


data SemState {
    sstGlob :: GlobEnv,
    sstStructTmpls :: TemplateMap Sid StructDesc
    sstClassTmpls :: TemplateMap Cid ClassDesc

data Template a = Tmpl a
                | TmplS Int (Sid -> Template a)
                | TmplC Int (Cid -> Template a)

type TemplateMap a b = (M.Map a (Template a), M.Map [Either Sid Cid] a)

type StructTemplate = Template StructDesc
type ClassTemplate = Template ClassDesc


-- To samo mozna by dostac uzywajac ErrorT, StateT i Reader,
-- ale ilosc liftow bylaby przytlaczajaca - zaimplementowanie
-- tego w jednym kawalku eliminuje je wszystkie.
newtype Semantic a = Semantic {
    runSemantic :: SemState -> LocEnv -> Either SemanticError (SemState, LocEnv, a)
}

instance Monad (Semantic) where
    esr1 >>= esr2 = Semantic $ \st env -> do
        (st', env', res1) <- runSemantic esr1 st env
        runSemantic (esr2 res1) st' env'
    return x = Semantic $ \env st -> return (env, st, x)

instance (Error SemanticError) => MonadError SemanticError (Semantic) where
    throwError e = Semantic $ \_ _ -> throwError e
    (Semantic try) `catchError` h = Semantic $ \st e -> do
        (try st e) `catchError` (\err -> runSemantic (h err) st e)

instance MonadState SemState (Semantic) where
    get = Semantic $ \st env -> return (st, env, st)
    put st = Semantic $ \_ env -> return (st, env, ())

instance MonadReader LocEnv (Semantic) where
    ask = Semantic $ \st env -> return (st, env, env)
    local f (Semantic run) = Semantic $ \st env -> do
        (st', _, res) <- run st (f env)
        return (st', env, res)


data SemanticError = SemanticError String

instance Error SemanticError where
  strMsg = SemanticError

showSemError :: SemanticError -> String
showSemError (SemanticError s) = "SemanticError: " ++ s


moduleSem :: Module -> SemanticErrorOr (IO ())
moduleSem (Module_ stmts) = do
    exe <- stmtBlockSem (StatementBlock_ stmts) globalEnv
    return $ do
        let runState exe emptyMem
    where

        globalEnv :: Env
        globalEnv = Env {
            envSuper=Nothing,
            envVars=M.fromList [],
            envClasses=M.fromList [],
        --             ("Type", classType),
        --             ("String", classString),
        --             ("Index", classIndex)
        --         ],
            envStructs=M.fromList [
                    ("Type", structType)
                ],
            envExpectedReturnType=Nothing
        } where
        --     classType = ClassDesc {
        --         classProps=M.fromList [
        --             ("name", PropDesc {
        --                 propGetter=Just classString,
        --                 propSetter=Nothing
        --             })
        --         ],
        --         classMths=M.fromList []
        --     }
            structType = StructDesc {
                structAttrs=M.fromList [],
                structMths=M.fromList [],
                structClasses=M.fromList [
                    ("Type", Impl {
                        implProps=M.fromList [],
                        implMths=M.fromList []
                    })
                ]
            }

        -- functionClass :: [VarType] -> VarType -> ClassDesc
        -- functionClass argTypes returnType = do
        --     ClassDesc {
        --         classProps=M.empty,
        --         classMths=M.fromList [
        --             ("call", MthDesc {
        --                 mthRetType=returnType,
        --                 mthArgs=map (\tp -> ("?", tp, Nothing)) argTypes
        --             })
        --         ]
        --     }

        emptyMem :: Memory
        emptyMem = Memory {
            memFid=0,
            memValues=M.empty,
            memFrames=M.fromList [(0, globFrame)]
        } where
            globFrame = Frame{
                frameParentId=0-1,
                frameContent=M.empty
            }

stmtBlockSem :: StatementBlock -> Env -> SemanticErrorOr (Cont -> Cont)
stmtBlockSem (StatementBlock_ stmts) outerEnv = do
    hoistedEnv <- return outerEnv  -- TODO: hoisting
    (_, kk) <- foldl f (return (hoistedEnv, id)) stmts
    return kk
    where
        f prev stmt = do
            (env, sem1) <- prev
            (env', sem2) <- stmtSem stmt env
            return (env', sem1.sem2)

stmtSem :: Stmt -> Env -> SemanticErrorOr (Env, (Cont -> Cont))
stmtSem (Stmt_Expr expr) env = do
    esem <- exprSem expr env
--     return (env, esem.const)  -- tak to docelowo ma wygladac, ale debugowo wypisuje wartosci
    return (env, esem.(\k v m -> (hPutStrLn stderr $ "Stmt_Expr: " ++ (show v)) >> (k m)))

stmtSem _ _ = undefined

exprSem :: Expr -> Env -> SemanticErrorOr (ContE -> Cont)
exprSem (Expr_Char c) _ = return ($ (ValChar c))
-- exprSem (Expr_String c) _ =
-- exprSem (Expr_Double d) _ = return ($ (ValDouble d))
exprSem (Expr_Int i) _ = return ($ (ValInt $ fromInteger i))
-- exprSem (Expr_Tuple _) _ = ???
-- exprSem (Expr_Array _) _ = ???
exprSem (Expr_Var (LowerIdent (_, v))) _ = do
    -- TODO: check env
    return $ \ke m -> ke (getVar v m) m
-- exprSem (Expr_Type _) _ = ???

exprSem (Expr_Add exp1 exp2) env = intBinopSem (+) exp1 exp2 env
exprSem (Expr_Sub exp1 exp2) env = intBinopSem (-) exp1 exp2 env
exprSem (Expr_Mul exp1 exp2) env = intBinopSem (*) exp1 exp2 env
-- exprSem (Expr_Div exp1 exp2) env = ??? / exp1 exp2 env
exprSem (Expr_IntDiv exp1 exp2) env = intBinopSem div exp1 exp2 env
exprSem (Expr_Mod exp1 exp2) env = intBinopSem mod exp1 exp2 env

-- Expr_Minus.     Expr8 ::= "-" Expr8 ;
-- Expr_Plus.      Expr8 ::= "+" Expr8 ;

exprSem _ _ = undefined

intBinopSem :: (Int -> Int -> Int) -> Expr -> Expr -> Env ->
    SemanticErrorOr (ContE -> Cont)
intBinopSem op exp1 exp2 env = do
    e1sem <- exprSem exp1 env
    e2sem <- exprSem exp2 env
    return $ \ke -> e1sem $ \v1 -> e2sem $ \v2 ->
                ke $ ValInt $ (valToInt v1) `op` (valToInt v2)

declSem :: Decl -> Env -> SemanticErrorOr (Env, (Cont -> Cont))
declSem = undefined
