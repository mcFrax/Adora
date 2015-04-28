{-# OPTIONS -XMultiParamTypeClasses #-}

module Semantic where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import qualified Data.Map as M

-- import System.Exit(exitFailure)
import System.IO

import Absadora

import Memory
import Types


data SemState = SemState {
    sstGlob :: GlobEnv,
    sstStructTmpls :: TemplateMap Sid StructDesc,
    sstClassTmpls :: TemplateMap Cid ClassDesc,
    sstFunClasses :: M.Map () Cid
}

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

instance Monad Semantic where
    esr1 >>= esr2 = Semantic $ \st env -> do
        (st', env', x) <- runSemantic esr1 st env
        runSemantic (esr2 x) st' env'
    return x = Semantic $ \st env -> return (st, env, x)

instance MonadError SemanticError Semantic where
    throwError e = Semantic $ \_ _ -> throwError e
    (Semantic try) `catchError` h = Semantic $ \st e -> do
        (try st e) `catchError` (\err -> runSemantic (h err) st e)

instance MonadState SemState Semantic where
    get = Semantic $ \st env -> return (st, env, st)
    put st = Semantic $ \_ env -> return (st, env, ())

instance MonadReader LocEnv Semantic where
    ask = Semantic $ \st env -> return (st, env, env)
    local f (Semantic run) = Semantic $ \st env -> do
        (st', _, res) <- run st (f env)
        return (st', env, res)

setEnv :: LocEnv -> Semantic ()
setEnv env = Semantic $ \st _ -> return (st, env, ())

modifyEnv :: (LocEnv -> LocEnv) -> Semantic ()
modifyEnv f = setEnv =<< (liftM f ask)


data SemanticError = SemanticError String

instance Error SemanticError where
  strMsg = SemanticError

showSemError :: SemanticError -> String
showSemError (SemanticError s) = "SemanticError: " ++ s


moduleSem :: Module -> Either SemanticError (IO ())
moduleSem (Module_ stmts) = do
    (sst, _, sem) <- runSemantic (stmtSeqSem stmts) initSemState outerEnv
    let runEnv = RunEnv {
        reStructs=globStructs $ sstGlob sst,
        reReturn=undefined,
        reBreak=undefined,
        reContinue=undefined
    }
    return $ runExe sem runEnv emptyMem
    where
        initSemState = SemState {
            sstGlob=GlobEnv {
                globClasses=M.empty,
                globStructs=M.empty
            },
            sstStructTmpls=(M.empty, M.empty),
            sstClassTmpls=(M.empty, M.empty),
            sstFunClasses=M.empty
        }

        outerEnv = LocEnv {
            envSuper=Nothing,
            envVars=M.fromList [],
            envClasses=M.fromList [],
            envStructs=M.fromList [],
            envExpectedReturnType=Nothing
        }

        emptyMem = Memory {
            memFid=0,
            memValues=M.empty,
            memFrames=M.fromList [(0, globFrame)]
        } where
            globFrame = Frame{
                frameParentId=Nothing,
                frameContent=M.empty
            }

stmtBlockSem :: StatementBlock -> Semantic (Exe ())
stmtBlockSem (StatementBlock_ stmts) = stmtSeqSem stmts

stmtSeqSem :: [Stmt] -> Semantic (Exe ())
stmtSeqSem stmts = do
    outerEnv <- ask
    hoistedEnv <- return outerEnv  -- TODO: hoisting
    local (const hoistedEnv) $ stmtSeqSem' stmts
    where
        stmtSeqSem' [] = return noop
        stmtSeqSem' (h:t) = do
            hexe <- stmtSem h
            modifiedEnv <- ask  -- TODO: env modification
            texe <- local (const modifiedEnv) $ stmtSeqSem' t
            return $ hexe.const.texe

stmtSem :: Stmt -> Semantic (Exe ())
stmtSem (Stmt_Decl _) = return noop

stmtSem (Stmt_Expr expr) = do
    esem <- exprSem expr
    return $ \k -> do
        -- esem $ \_ -> k ()
        rValue esem $ \v -> do
            io (hPutStrLn stderr $ "Stmt_Expr: " ++ (show v)) k

stmtSem (Stmt_Let (LowerIdent (_, varName)) expr) = do
    eexe <- exprSem expr
--     check for shadowing ...
--     modifyEnv ...
    return $ \k -> do
        rValue eexe $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Var (LowerIdent (_, varName)) expr) = do
    eexe <- exprSem expr
--     check for shadowing ...
--     modifyEnv ...
    return $ \k -> do
        rValue eexe $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Assign lexpr AssignOper_Assign rexpr) = do
    lexe <- exprSem lexpr
    rexe <- exprSem rexpr
--     typecheck  ...
    return $ \k -> do
        lValue lexe $ \pt -> do
            rValue rexe $ \val re mem -> do
                k () re $ memSet mem pt val

stmtSem (Stmt_If condexpr block elses) = do
    exeCond <- exprSem condexpr
    exeThen <-stmtBlockSem block
    exeElse <- elseSem elses
    return $ \k ->
        rValue exeCond $ \condVal ->
            if isTruthy condVal then
                exeThen k
            else
                exeElse k
    where
        elseSem ElseClauses_None = return noop
        elseSem (ElseClauses_Else elseBlock) = stmtBlockSem elseBlock
        elseSem (ElseClauses_Elif elifcondexpr elseBlock moreElses) = do
            stmtSem (Stmt_If elifcondexpr elseBlock moreElses)

-- Stmt_LetTuple.      Stmt ::= "let" "(" [LowerIdent] ")" "=" Expr ;  -- tuple unpacking
-- Stmt_Return.        Stmt ::= "return" ;
-- Stmt_ReturnValue.   Stmt ::= "return" Expr ;
-- Stmt_Case.          Stmt ::= "case" Expr "class" "of" "{" [CaseClause] "}";
-- Stmt_While.         Stmt ::= "while" Expr  StatementBlock ;
-- Stmt_ForIn.         Stmt ::= "for" LowerIdent "in" Expr StatementBlock ;
-- Stmt_For.           Stmt ::= "for" LowerIdent "=" Expr "then" Expr StatementBlock ;
-- Stmt_ForWhile.      Stmt ::= "for" LowerIdent "=" Expr "then" Expr "while" Expr StatementBlock ;
-- Stmt_Break.         Stmt ::= "break" ;
-- Stmt_Continue.      Stmt ::= "continue" ;
-- Stmt_Assert.        Stmt ::= "assert" Expr ;
stmtSem stmt = notYet stmt


data ExeExpr = RValue (Exe Value)
             | LValue { lValue :: Exe Pointer }

rValue :: ExeExpr -> (Exe Value)
rValue (RValue e) ke = e ke
rValue (LValue e) ke = e $ \pt re mem -> ke (memGet mem pt) re mem

exeExpr :: ExeExpr -> Exe ()
exeExpr (RValue e) = e.const.noop
exeExpr (LValue e) = e.const.noop

isTruthy :: Value -> Bool
isTruthy = valToBool
-- TODO: isTruthy for other values


exprSem :: Expr -> Semantic ExeExpr
exprSem (Expr_Char c) = return $ RValue ($ ValChar c)
-- exprSem (Expr_String c) _ =
-- exprSem (Expr_Double d) _ = liftM RValue $ return $ ValDouble d
exprSem (Expr_Int i) = return $ RValue ($ ValInt $ fromInteger i)
-- exprSem (Expr_Tuple _) _ = ???
-- exprSem (Expr_Array _) _ = ???
exprSem (Expr_Var (LowerIdent (_, varName))) = do
    -- TODO: check env
    return $ LValue $ \ke re mem -> ke (getVarPt varName mem) re mem
-- exprSem (Expr_Type _) _ = ???

exprSem (Expr_RelOper exp1 RelOper_Eq exp2) = do
    -- TODO: laczenie operatorow,
    e1exe <- exprSem exp1
    e2exe <- exprSem exp2
    return $ RValue $ \ke -> do
        rValue e1exe $ \(ValInt v1) -> do
            rValue e2exe $ \(ValInt v2) -> do
                ke (ValBool $ v1 == v2)

exprSem (Expr_Add exp1 exp2) = intBinopSem (+) exp1 exp2
exprSem (Expr_Sub exp1 exp2) = intBinopSem (-) exp1 exp2
exprSem (Expr_Mul exp1 exp2) = intBinopSem (*) exp1 exp2
-- exprSem (Expr_Div exp1 exp2) = ??? / exp1 exp2
exprSem (Expr_IntDiv exp1 exp2) = intBinopSem div exp1 exp2
exprSem (Expr_Mod exp1 exp2) = intBinopSem mod exp1 exp2

-- Expr_Minus.     Expr8 ::= "-" Expr8 ;
-- Expr_Plus.      Expr8 ::= "+" Expr8 ;

exprSem expr = notYet expr


intBinopSem :: (Int -> Int -> Int) -> Expr -> Expr -> Semantic ExeExpr
intBinopSem op exp1 exp2 = do
    e1exe <- exprSem exp1
    e2exe <- exprSem exp2
    return $ RValue $ \ke -> do
        rValue e1exe $ \(ValInt v1) -> do
            rValue e2exe $ \(ValInt v2) -> do
                ke $ ValInt $ op v1 v2


declSem :: Decl -> Semantic (Exe ())
declSem decl = notYet decl

notYet :: Show a => a -> Semantic b
notYet = throwError.(SemanticError).("not yet: " ++).show
