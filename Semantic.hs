{-# OPTIONS -XMultiParamTypeClasses #-}

module Semantic where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import qualified Data.Map.Strict as M

import System.Exit(exitFailure)
import System.IO

import Absadora
import Printadora(printTree)

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
            return $ mkExe $ (exec hexe).const.(exec texe)

stmtSem :: Stmt -> Semantic (Exe ())
stmtSem (Stmt_Decl _) = return noop

stmtSem (Stmt_Expr expr) = do
    esem <- exprSem expr
    return $ mkExe $ \k -> do
        -- esem $ \_ -> k ()
        exec (rValue esem) $ \v -> do
            io (hPutStrLn stderr $ "Stmt_Expr: " ++ (show v)) k

stmtSem (Stmt_Let (LowerIdent (_, varName)) expr) = do
    eexe <- exprSem expr
--     check for shadowing ...
--     modifyEnv ...
    return $ mkExe $ \k -> do
        exec (rValue eexe) $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Var (LowerIdent (_, varName)) expr) = do
    eexe <- exprSem expr
--     check for shadowing ...
--     modifyEnv ...
    return $ mkExe $ \k -> do
        exec (rValue eexe) $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Assign lexpr AssignOper_Assign rexpr) = do
    lexe <- exprSem lexpr
    rexe <- exprSem rexpr
--     typecheck  ...
    return $ mkExe $ \k -> do
        exec (lValue lexe) $ \pt -> do
            exec (rValue rexe) $ \val re mem -> do
                k () re $ memSet mem pt val

stmtSem (Stmt_If condexpr block elses) = do
    exeCond <- exprSem condexpr
    exeThen <-stmtBlockSem block
    exeElse <- elseSem elses
    return $ mkExe $ \k ->
        exec (rValue exeCond) $ \condVal ->
            if isTruthy condVal then
                exec exeThen k
            else
                exec exeElse k
    where
        elseSem ElseClauses_None = return noop
        elseSem (ElseClauses_Else elseBlock) = stmtBlockSem elseBlock
        elseSem (ElseClauses_Elif elifcondexpr elseBlock moreElses) = do
            stmtSem (Stmt_If elifcondexpr elseBlock moreElses)

stmtSem (Stmt_While condexpr block) = do
    exeCond <- exprSem condexpr
    exeBody <-stmtBlockSem block
    return $ fix $ \exeLoop -> do
        mkExe $ \k re -> do
            let popRe re'' = re''{
                reBreak=reBreak re,
                reContinue=reContinue re
            }
            let re' = re{
                reBreak=(k ()).popRe,
                reContinue=(exec exeLoop k).popRe
            }
            let k' = exec (rValue exeCond) $ \condVal _ -> do
                if isTruthy condVal then
                    exec exeBody (\_ -> (exec exeLoop k).popRe) re'
                else
                    k () re
            k' re'
    where
        fix :: (a -> a) -> a
        fix f = f $ fix f

stmtSem Stmt_Break = return $ mkExe $ \_ re -> reBreak re re
stmtSem Stmt_Continue = return $ mkExe $ \_ re -> reContinue re re
stmtSem Stmt_Return = return $ mkExe $ \_ re -> reReturn re ValNull re
stmtSem (Stmt_ReturnValue expr) = do
    eexe <- exprSem expr
    return $ mkExe $ \_ -> exec (rValue eexe) $ \val re -> reReturn re val re
stmtSem (Stmt_Assert expr) = do
    eexe <- exprSem expr
    return $ mkExe $ \k -> exec (rValue eexe) $ \val -> do
        if isTruthy val then
            k ()
        else
            \_ _ -> do
                hPutStrLn stderr $ "Assertion failed: " ++ (printTree expr)
                exitFailure

-- Stmt_LetTuple.      Stmt ::= "let" "(" [LowerIdent] ")" "=" Expr ;  -- tuple unpacking
-- Stmt_Case.          Stmt ::= "case" Expr "class" "of" "{" [CaseClause] "}";
-- Stmt_ForIn.         Stmt ::= "for" LowerIdent "in" Expr StatementBlock ;
-- Stmt_For.           Stmt ::= "for" LowerIdent "=" Expr "then" Expr StatementBlock ;
-- Stmt_ForWhile.      Stmt ::= "for" LowerIdent "=" Expr "then" Expr "while" Expr StatementBlock ;
stmtSem stmt = notYet stmt


data ExeExpr = RValue (Exe Value)
             | LValue { lValue :: Exe Pointer }

rValue :: ExeExpr -> (Exe Value)
rValue (RValue e) = e
rValue (LValue e) = mkExe $ \ke -> exec e $ \pt re mem -> ke (memGet mem pt) re mem

exeExpr :: ExeExpr -> Exe ()
exeExpr (RValue e) = mkExe $ (exec e).const.(exec noop)
exeExpr (LValue e) = mkExe $ (exec e).const.(exec noop)

isTruthy :: Value -> Bool
isTruthy = valToBool
-- TODO: isTruthy for other values


exprSem :: Expr -> Semantic ExeExpr
exprSem (Expr_Char c) = return $ RValue $ mkExe ($ ValChar c)
-- exprSem (Expr_String c) _ =
-- exprSem (Expr_Double d) _ = liftM RValue $ return $ ValDouble d
exprSem (Expr_Int i) = return $ RValue $ mkExe ($ ValInt $ fromInteger i)
-- exprSem (Expr_Tuple _) _ = ???
-- exprSem (Expr_Array _) _ = ???
exprSem (Expr_Var (LowerIdent (_, varName))) = do
    -- TODO: check env
    return $ LValue $ mkExe $ \ke re mem -> ke (getVarPt varName mem) re mem
-- exprSem (Expr_Type _) _ = ???

exprSem (Expr_RelOper exp1 RelOper_Eq exp2) = do
    -- TODO: laczenie operatorow,
    e1exe <- exprSem exp1
    e2exe <- exprSem exp2
    return $ RValue $ mkExe $ \ke -> do
        exec (rValue e1exe) $ \(ValInt v1) -> do
            exec (rValue e2exe) $ \(ValInt v2) -> do
                ke (ValBool $ v1 == v2)

exprSem (Expr_Add exp1 exp2) = intBinopSem (+) exp1 exp2
exprSem (Expr_Sub exp1 exp2) = intBinopSem (-) exp1 exp2
exprSem (Expr_Mul exp1 exp2) = intBinopSem (*) exp1 exp2
-- exprSem (Expr_Div exp1 exp2) = ??? / exp1 exp2
exprSem (Expr_IntDiv exp1 exp2) = intBinopSem div exp1 exp2
exprSem (Expr_Mod exp1 exp2) = intBinopSem mod exp1 exp2

-- Expr_Minus.     Expr8 ::= "-" Expr8 ;
-- Expr_Plus.      Expr8 ::= "+" Expr8 ;

exprSem (Expr_Lambda signature block) = do
    (fnSgn, defArgs) <- fnSigSem signature
    exeBody <-stmtBlockSem block
    let exeFunction args = mkExe $ \ke re0 mem0 -> do
        let popCall k re mem = k re{reReturn=reReturn re0} (popFrame mem)
        let re1 = re0{reReturn=(\val -> popCall $ ke val)}
        let (fid, mem1) = allocFrame mem0
        let foldArg (name, exeArgPt) k = do
                exec exeArgPt $ \pt re mem -> do
                    k re $ allocFrameVar fid name pt mem
        let boundArgs = map (\(Just name, exe) -> (name, exe)) args -- TODO
        let bodyCont re mem = (exec exeBody $ const noReturn) re $ mem{memFid=fid}
        foldr foldArg bodyCont boundArgs re1 mem1
        -- TODO: statycznie wymusic wywolanie return w funkcji, "prawdziwe" lub sztuczne
        where
            noReturn = error "No return in function"

    return $ RValue $ mkExe $ \ke -> ke $ ValFunction $ FunImpl {
        funDesc=fnSgn,
        funBody=exeFunction
    }

exprSem (Expr_FunCall expr args) = do
    exeFn <- exprSem expr
    argExes <- mapM argSem args
    return $ RValue $ mkExe $ \ke -> do
        exec (rValue exeFn) $ \(ValFunction fnImpl) -> exec (funBody fnImpl argExes) ke
    where
        argSem (FunCallArg_Positional argExpr) = do
            exeArg <- exprSem argExpr
            return (Nothing, exeExprToExePt exeArg)
        argSem (FunCallArg_Keyword (LowerIdent (_, name)) argExpr) = do
            exeArg <- exprSem argExpr
            return (Just name, exeExprToExePt exeArg)

exprSem expr = notYet expr

exeExprToExePt :: ExeExpr -> (Exe Pointer)
exeExprToExePt exeVal = do
    mkExe $ \kpt -> do
        exec (rValue exeVal) $ \val re mem -> do
            let (pt, mem') = alloc val mem
            kpt pt re mem'


fnSigSem :: FnSignature -> Semantic (FunSgn, M.Map VarName (Exe Pointer))
fnSigSem (FnSignature_ args _) = do
    argTuples <- mapM argSem args
    -- TODO: process ret type
    -- OptResultType_None. OptResultType ::= ;
    -- OptResultType_Some. OptResultType ::= "->" TypeExpr;
    let funSgn = FunSgn {
        mthRetType=undefined,
        mthArgs=(map fst argTuples)
    }
    let defArgsMap = foldl argToMap M.empty argTuples
    return (funSgn, defArgsMap)
    where
        argSem :: FunDef_Arg -> Semantic (ArgSgn, Maybe (Exe Pointer))
        argSem (FunDef_Arg_Named _ (LowerIdent (_, name)) defVal) = do
            -- TODO: process type
            (hasDef, defExe) <- case defVal of
                MaybeDefaultVal_None -> return (False, Nothing)
                MaybeDefaultVal_Some expr -> do
                    exee <- exprSem expr
                    let exe = mkExe $ \kpt -> exec (rValue exee) $ \val re mem -> do
                        let (pt, mem') = alloc val mem
                        kpt pt re mem'
                    return (True, Just exe)
            return $ (ArgSgn {
                argName=Just name,
                argType=undefined,
                argHasDefault=hasDef
            }, defExe)
        argSem (FunDef_Arg_Unnamed _) = do
            -- TODO: process type
            return $ (ArgSgn {
                argName=Nothing,
                argType=undefined,
                argHasDefault=False
            }, Nothing)
        argToMap m (_, Nothing) = m
        argToMap m (ArgSgn{argName=Just name}, Just exe) = M.insert name exe m
        argToMap _ _ = error "argToMap incomplete"


intBinopSem :: (Int -> Int -> Int) -> Expr -> Expr -> Semantic ExeExpr
intBinopSem op exp1 exp2 = do
    e1exe <- exprSem exp1
    e2exe <- exprSem exp2
    return $ RValue $ mkExe $ \ke -> do
        exec (rValue e1exe) $ \(ValInt v1) -> do
            exec (rValue e2exe) $ \(ValInt v2) -> do
                ke $ ValInt $ op v1 v2


declSem :: Decl -> Semantic (Exe ())
declSem decl = notYet decl

notYet :: Show a => a -> Semantic b
notYet = throwError.(SemanticError).("not yet: " ++).show
