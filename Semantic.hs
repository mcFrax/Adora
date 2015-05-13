{-# OPTIONS -XMultiParamTypeClasses #-}

module Semantic where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

import System.Exit(exitFailure)
import System.IO

import Absadora
import Printadora(printTree)

import Memory
import Types


data SemState = SemState {
    sstGlob :: GlobEnv,
    sstStructTmpls :: TemplateMap STid StructDesc,
    sstClassTmpls :: TemplateMap CTid ClassDesc,
    sstFunClasses :: M.Map () Cid,
    sstNextCid :: Cid,
    sstClassRules :: [ClassRule]
}

data ClassRule = ClassRule

data Template a = Tmpl0 a
                | TmplS Int (Sid -> Template a)
                | TmplC Int (Cid -> Template a)

type TemplateMap a b = (M.Map a (Template a), M.Map [Either Sid Cid] b)

type StructTemplate = Template StructDesc
type ClassTemplate = Template ClassDesc

addRule :: ClassRule -> Semantic ()
addRule rule = do
    modify $ \stt -> stt{sstClassRules=rule:(sstClassRules stt)}

newCid :: Semantic Cid
newCid = do
    stt <- get
    let cid = sstNextCid stt
    put stt{sstNextCid=cid+1}
    return cid


-- To samo mozna by dostac uzywajac ErrorT, StateT i Reader,
-- ale ilosc liftow bylaby przytlaczajaca - zaimplementowanie
-- tego w jednym kawalku eliminuje je wszystkie.
newtype Semantic a = Semantic {
    runSemantic :: SemState -> LocEnv -> Either SErr (SemState, LocEnv, a)
}

instance Monad Semantic where
    esr1 >>= esr2 = Semantic $ \st env -> do
        (st', env', x) <- runSemantic esr1 st env
        runSemantic (esr2 x) st' env'
    return x = Semantic $ \st env -> return (st, env, x)

instance MonadError SErr Semantic where
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


data SErr = SErr String
          | SErrP (Int, Int) String

instance Error SErr where
  strMsg = SErr

showSemError :: SErr -> String
showSemError (SErr s) = "?:?: error: " ++ s
showSemError (SErrP (ln, col) s) = do
    (show ln) ++ ":" ++ (show col) ++ ": error: " ++ s

boolCid :: Cid
boolCid = 0-1

intCid :: Cid
intCid = 0-2

doubleCid :: Cid
doubleCid = 0-3

charCid :: Cid
charCid = 0-4

typeCid :: Cid
typeCid = 0-5

voidCid :: Cid
voidCid = 0-100

fooCid :: Cid
fooCid = 123

mkMth :: (Pointer -> FunImpl) -> PropImpl
mkMth mth = PropImpl {
    propGetter=(\pt -> mkExeV $ ($ ValFunction $ mth pt)),
    propSetter=(error "Methods are read-only properties")
}

moduleSem :: Module -> Either SErr (IO ())
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
                globClasses=M.fromList [
                    (fooCid, ClassDesc {
                        className="Foo",
                        classProps=M.fromList [
                            ("foo", VarType {
                                varMutable=True,
                                varClass=intCid
                            })
                        ],
                        classMths=M.fromList [
                            ("fooThat", FunSgn {
                                mthRetType=Just intCid,
                                mthArgs=[
                                    ArgSgn {
                                        argName=Nothing,
                                        argType=intCid,
                                        argHasDefault=False
                                    }
                                ]
                            })
                        ]
                    })
                ],
                globStructs=M.fromList [
                    (fooCid, StructDesc {
                        structCid=fooCid,
                        structAttrs=M.empty,  -- :: M.Map VarName Cid,
                        structClasses=M.fromList [  -- :: M.Map Cid Impl
                            (fooCid, M.fromList [  -- :: M.Map VarName PropImpl,
                                ("foo", PropImpl {
                                    propGetter=(\_pt -> mkExeV $ \ke -> ke $ ValInt 666),
                                    propSetter=(\_pt vpt -> mkExe $ \k re mem -> (hPutStrLn stderr $ (++) "foo setter: " $ show $ memGet mem vpt) >> (k () re mem))
                                }),
                                ("fooThat", mkMth $ \_pt -> FunImpl {
                                    funDesc=FunSgn {
                                        mthRetType=Just intCid,
                                        mthArgs=[]
                                    },
                                    funBody=(\args -> mkExeV $ \ke re mem -> (hPutStrLn stderr $ (++) "fooThat: " $ show $ map fst args) >> (ke (ValInt 555) re mem))
                                })
                            ])
                        ]
                    })
                ]
            },
            sstStructTmpls=(M.empty, M.empty),
            sstClassTmpls=(M.empty, M.empty),
            sstFunClasses=M.empty,
            sstNextCid=0,
            sstClassRules=[]
        }

        outerEnv = LocEnv {
            envSuper=Nothing,
            envVars=M.fromList [],
            envClasses=M.fromList [
                ("Foo", fooCid)
            ],
            envStructs=M.fromList [
                ("Foo", fooCid)
            ],
            envExpectedReturnType=Nothing,
            envInsideLoop=False
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
    hoisted stmts $ stmtSeqSem' stmts
    where
        stmtSeqSem' [] = return noop -- TODO: check return
        stmtSeqSem' (h:t) = do
            (hexe, t') <- case h of
                Stmt_If (Tok_If (pos, _)) condExpr bodyBlock -> do
                    let (elses, t') = stripElses t
                    hexe <- ifSem condExpr bodyBlock elses
                    return (hexe, t')
                _ -> do
                    hexe <- stmtSem h
                    return (hexe, t)
            modifiedEnv <- ask  -- TODO: env modification (including var initialization)
            texe <- local (const modifiedEnv) $ stmtSeqSem' t'
            return $ mkExe $ (exec hexe).const.(exec texe)
        stripElses ((Stmt_Elif (Tok_Elif (pos, _)) condExpr bodyBlock):t) = let
            ((elifs, maybeElse), t') = stripElses t
            in (((condExpr, bodyBlock):elifs, maybeElse), t')
        stripElses ((Stmt_Else (Tok_Else (pos, _)) bodyBlock):t) = do
            (([], Just bodyBlock), t)
        stripElses t = do
            (([], Nothing), t)
        ifSem condExpr bodyBlock elses = do
            exeCond <- exprSem condExpr
            exeThen <-stmtBlockSem bodyBlock
            exeElse <- elseSem elses
            return $ mkExe $ \k ->
                rValue exeCond $ \condVal ->
                    if isTruthy condVal then
                        exec exeThen k
                    else
                        exec exeElse k
            where
                elseSem ([], Nothing) = return noop
                elseSem ([], Just elseBody) = stmtBlockSem elseBody
                elseSem (((condExpr', bodyBlock'):elifs), maybeElse) = do
                    ifSem condExpr' bodyBlock' (elifs, maybeElse)

hoisted :: [Stmt] -> Semantic (Exe a) -> Semantic (Exe a)  -- wrapped `local`
hoisted stmts innerSem = do
    outerEnv <- ask
    env' <- foldl (\a x -> a >>= (hoistLocStmt x)) (return outerEnv) stmts
    local (const env') $ do
        mapM_ hoistGlobStmt stmts
        innerSem
    where
        hoistLocStmt :: Stmt -> LocEnv -> Semantic LocEnv
        hoistLocStmt stmt beforeEnv = do
            return beforeEnv

        hoistLocDecl :: Decl -> LocEnv -> Semantic LocEnv
        hoistLocDecl stmt beforeEnv = do
            return beforeEnv

        hoistGlobStmt :: Stmt -> Semantic ()  -- updating GlobEnv
        hoistGlobStmt stmt = do
            return ()
        hoistGlobDecl :: Decl -> Semantic ()  -- updating GlobEnv
        hoistGlobDecl stmt = do
            return ()


data ExprSem = RValue {
                expCid :: Cid,
                expRValue :: Either (Exe Value) (Exe Pointer)
            } | LValue {
                expCid :: Cid,
                expRValue :: Either (Exe Value) (Exe Pointer),
                setLValue :: Pointer -> Exe ()
            } | TypeValue {
                expCid :: Cid,
                expRValue :: Either (Exe Value) (Exe Pointer), -- reflection only
                expCls :: Maybe (Either Cid CTid),
                expStr :: Maybe (Either Cid CTid)
            }

rValue :: ExprSem -> SemiCont Value
rValue = execV.expRValue

rValuePt :: ExprSem -> SemiCont Pointer
rValuePt = execPt.expRValue

execV :: Either (Exe Value) (Exe Pointer) -> SemiCont Value
execV (Left exeVal) = exec exeVal
execV (Right exePt) = \ke -> do
    exec exePt $ \pt re mem -> do
        ke (memGet mem pt) re mem

execPt :: Either (Exe Value) (Exe Pointer) -> SemiCont Pointer
execPt (Left exeVal) = \kpt -> do
    exec exeVal $ \val re mem -> do
        let (pt, mem') = alloc val mem
        kpt pt re mem'
execPt (Right exePt) = exec exePt

mkExeV :: SemiCont Value -> Either (Exe Value) (Exe Pointer)
mkExeV = (Left).mkExe

mkExePt :: SemiCont Pointer -> Either (Exe Value) (Exe Pointer)
mkExePt = (Right).mkExe

isTruthy :: Value -> Bool
isTruthy = valToBool
-- TODO: isTruthy for other values



stmtSem :: Stmt -> Semantic (Exe ())
stmtSem (Stmt_Decl _) = return noop

stmtSem (Stmt_Expr expr) = do
    esem <- exprSem expr
    return $ mkExe $ \k -> do
        -- esem $ \_ -> k ()
        rValue esem $ \v -> do
            io (hPutStrLn stderr $ "Stmt_Expr: " ++ (show v)) k

stmtSem (Stmt_Let (LowerIdent (_, varName)) expr) = do
    eexe <- exprSem expr
--     check for shadowing ...
--     modifyEnv ...
    return $ mkExe $ \k -> do
        rValue eexe $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Var (LowerIdent (_, varName)) expr) = do
    eexe <- exprSem expr
--     check for shadowing ...
--     modifyEnv ...
    return $ mkExe $ \k -> do
        rValue eexe $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Assign lexpr AssignOper_Assign rexpr) = do
    lexe <- exprSem lexpr
    rexe <- exprSem rexpr
--     typecheck  ...
    case lexe of
         LValue {} -> do
            return $ mkExe $ \k -> do
                rValuePt rexe $ \pt -> exec (setLValue lexe pt) k
         _ -> throwError $ SErr $ "Left side of assignment is not l-value"

stmtSem (Stmt_If {}) = error "Stmt_If should have been handled by stmtSeqSem"
stmtSem (Stmt_Elif {}) = throwError $ SErr $ "Unexpected elif statement"
stmtSem (Stmt_Else {}) = throwError $ SErr $ "Unexpected else statement"

stmtSem (Stmt_While condExpr block) = do
    exeCond <- exprSem condExpr
    exeBody <-local (\e -> e{envInsideLoop=True}) $ stmtBlockSem block
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
            let k' = rValue exeCond $ \condVal _ -> do
                if isTruthy condVal then
                    exec exeBody (\_ -> (exec exeLoop k).popRe) re'
                else
                    k () re
            k' re'
    where
        fix :: (a -> a) -> a
        fix f = f $ fix f

stmtSem Stmt_Break = do
    insideLoop <- asks envInsideLoop
    if insideLoop then
        return $ mkExe $ \_ re -> reBreak re re
    else
        throwError $ SErr $ "Unexpected break statement"
stmtSem Stmt_Continue = do
    insideLoop <- asks envInsideLoop
    if insideLoop then
        return $ mkExe $ \_ re -> reContinue re re
    else
        throwError $ SErr $ "Unexpected continue statement"
stmtSem (Stmt_Return (Tok_Return (pos, _))) = do
    rType <- asks envExpectedReturnType
    case rType of
        Just Nothing ->
            return $ mkExe $ \_ re -> reReturn re ValNull re
        Just cid ->
            throwError $ SErrP pos ("Return statement without value " ++
                                    "in funtion with return type")
        Nothing ->
            throwError $ SErrP pos ("Unexpected return statement " ++
                                    "outside of function body")
stmtSem (Stmt_ReturnValue (Tok_Return (pos, _)) expr) = do
    rType <- asks envExpectedReturnType
    case rType of
        Just Nothing ->
            throwError $ SErrP pos ("Return statement with value " ++
                                    "in funtion returning nothing")
        Just cid -> do
            -- TODO: check cid
            eexe <- exprSem expr
            return $ mkExe $ \_ -> rValue eexe $ \val re -> reReturn re val re
        Nothing ->
            throwError $ SErrP pos ("Unexpected return statement " ++
                                    "outside of function body")
stmtSem (Stmt_Assert expr) = do
    eexe <- exprSem expr
    return $ mkExe $ \k -> rValue eexe $ \val -> do
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


exprSem :: Expr -> Semantic ExprSem
exprSem (Expr_Char c) = return $ RValue charCid $ mkExeV ($ ValChar c)
-- exprSem (Expr_String c) _ =
-- exprSem (Expr_Double d) _ = liftM RValue $ return $ ValDouble d
exprSem (Expr_Int i) = return $ RValue intCid $ mkExeV ($ ValInt $ fromInteger i)
-- exprSem (Expr_Tuple _) _ = ???
-- exprSem (Expr_Array _) _ = ???
exprSem (Expr_Var (LowerIdent (_, varName))) = do
    -- TODO: check env
    let cid = intCid -- TODO TODO TODO
        exeGet = mkExePt $ \kpt re mem -> kpt (getVarPt varName mem) re mem
        exeSet pt = mkExe $ \k re mem -> k () re (assignFrameVar varName pt mem)
    return $ LValue cid exeGet exeSet

exprSem (Expr_Not expr) = do
    exee <- exprSem expr
    return $ RValue boolCid $ mkExeV $ \ke -> do
        rValue exee $ \(ValBool bval) -> do
            ke $ ValBool $ not bval

exprSem expr@(Expr_RelOper {}) = do
    sem' <- exprSem' expr
    return $ RValue boolCid $ mkExeV $ \ke -> do
        exec sem' $ \maybeVal -> do
            ke $ ValBool $ isJust maybeVal
    where
        exprSem' :: Expr -> Semantic (Exe (Maybe Value))
        exprSem' (Expr_RelOper exp1 oper exp2) = do
            e1exe <- exprSem' exp1
            e2exe <- exprSem exp2
            return $ mkExe $ \kmv -> do
                exec e1exe $ \maybeVal -> do
                    case maybeVal of
                        Just val1 -> do
                            rValue e2exe $ \val2 -> do
                                if operFun (valToInt val1) (valToInt val2) then
                                    kmv $ Just val2
                                else
                                    kmv Nothing
                        Nothing -> kmv Nothing
                where
                    operFun = case oper of
                        RelOper_Eq -> (==)
                        RelOper_Neq -> (/=)
                        RelOper_Lt -> (<)
                        RelOper_Lte -> (<=)
                        RelOper_Gt -> (>)
                        RelOper_Gte -> (>=)
                        _ -> undefined
        exprSem' expr' = do
            exee <- exprSem expr'
            return $ mkExe $ \kmv -> do
                rValue exee $ \v -> kmv $ Just v
--     exprSem' (Expr_RelOper exp1 oper exp2)
--     e1exe <- exprSem exp1
--     e2exe <- exprSem exp2
--     return $ RValue boolCid $ mkExeV $ \ke -> do
--         rValue e1exe $ \(ValInt v1) -> do
--             rValue e2exe $ \(ValInt v2) -> do
--                 ke (ValBool $ operFun v1 v2)

exprSem (Expr_Add exp1 exp2) = intBinopSem (+) exp1 exp2
exprSem (Expr_Sub exp1 exp2) = intBinopSem (-) exp1 exp2
exprSem (Expr_Mul exp1 exp2) = intBinopSem (*) exp1 exp2
-- exprSem (Expr_Div exp1 exp2) = ??? / exp1 exp2
exprSem (Expr_IntDiv exp1 exp2) = intBinopSem div exp1 exp2
exprSem (Expr_Mod exp1 exp2) = intBinopSem mod exp1 exp2
exprSem (Expr_Minus expr) = do
    exee <- exprSem expr
    cid <- newCid
    return $ RValue cid $ mkExeV $ \ke -> do
        rValue exee $ \(ValInt val) re mem -> do
            ke (ValInt $ 0 - val) re mem

exprSem (Expr_Lambda signature block) = do
    (fnSgn, defArgs) <- fnSignatureSem signature
    let makeInEnv outEnv = outEnv{
        envExpectedReturnType=Just $ mthRetType fnSgn,
        envInsideLoop=False
    }
    exeBody <- local makeInEnv $ stmtBlockSem block
    cid <- newCid
    let exeFunction closureFid argTuples = do
        mkExeV $ \ke re0 mem0 -> let
            doReturn val re mem = ke val re{reReturn=reReturn re0} (setFid (memFid mem0) mem)
            re1 = re0{reReturn=doReturn}
            (fid, mem1) = allocFrame closureFid mem0

            exeArgs :: Exe ()
            (exeArgs, defSgns) = foldl foldArg (noop, mthArgs fnSgn) argTuples

            foldArg (exe, argSgns) (maybeName, exeArgPt) = do
                (exe', argSgns')
                where
                    exe' = mkExe $ \ku -> do
                        exec exe $ \_ -> do
                            case maybeName' of
                                Just name -> do
                                    exec exeArgPt $ \pt re mem -> do
                                        ku () re $ allocFrameVar fid name pt mem
                                Nothing -> exec exeArgPt $ const $ ku ()
                    (maybeName', argSgns') = do
                        case maybeName of
                            Just _ -> (maybeName,
                                       filter ((/= maybeName).argName) argSgns)
                            Nothing -> (argName $ head argSgns,
                                        tail argSgns)
            exeDefArgs :: Exe ()
            exeDefArgs = do
                foldl foldDef noop defSgns
                where
                    foldDef exe (ArgSgn{argName=(Just name)}) = do
                        mkExe $ \ku -> do
                            exec exe $ \_ -> do
                                exec (defArgs M.! name) $ \pt re mem -> do
                                    ku () re $ allocFrameVar fid name pt mem
                    foldDef exe (ArgSgn{argName=Nothing}) = exe

            bodyCont :: Cont
            bodyCont re mem = do
--                 hPutStrLn stderr $ ("Call: length defSgns, M.size defArgs: " ++
--                                     (show $ length defSgns) ++ ", " ++
--                                     (show $ M.size defArgs))
                (exec exeBody $ const $ doReturn ValNull) re $ mem{memFid=fid}

            in (exec exeArgs $ const $ exec exeDefArgs $ const bodyCont) re1 mem1
            -- TODO: statycznie wymusic wywolanie return w funkcji, "prawdziwe" lub sztuczne

    let lambdaVal closureFid = ValFunction $ FunImpl {
        funDesc=fnSgn,
        funBody=exeFunction closureFid
    }
    return $ RValue cid $ mkExeV $ \ke re mem -> do
        ke (lambdaVal $ memFid mem) re mem

exprSem (Expr_TypeName (UpperIdent (_, typeName))) = do
    sid <- asks $ (!!! typeName).envStructs
    struct <- gets $ (!!! sid).globStructs.sstGlob
    let strCid = structCid struct
    cid <- newCid
    -- cid ==> ((??) -> <struct with cid=strCid>)
    let exeCtor args = mkExePt $ \kpt re mem -> do
        -- TODO: real constructor, common code with exprSem (Expr_Lambda ...)
        let pt = nextPtr mem
        let attrs = M.empty -- TODO
        let mem' = memSet mem pt $ ValObject {
            valObjStruct=sid,
            valObjAttrs=attrs
        }
        kpt pt re mem'

    return $ RValue cid $ mkExeV $ \ke -> ke $ ValFunction $ FunImpl {
        funDesc=FunSgn {
            mthRetType=Just strCid,
            mthArgs=[]
        },
        funBody=exeCtor
    }

exprSem (Expr_Field expr (LowerIdent (_, attrName))) = do
    exee <- exprSem expr
    let cid = intCid -- cid <- newCid
    let
        objCid = fooCid -- (expCid exee)
        exeGet = mkExePt $ exeAttr $ \objPt attrs kpt re mem -> do
            kpt (attrs M.! attrName) re mem
        exeSet valPt = mkExe $ exeAttr $ \objPt attrs k re mem -> let
            mem' = memAdjust mem objPt $ \objVal -> objVal{
                valObjAttrs=(M.insert attrName valPt attrs)
            }
            in k () re mem'
        exeAttr :: (Pointer -> M.Map VarName Pointer -> SemiCont a) -> SemiCont a
        exeAttr handler k_ =  do
            rValuePt exee $ \objPt re mem -> let
                attrs = valObjAttrs $ memGet mem objPt
                in handler objPt attrs k_ re mem
        in return $ LValue cid exeGet exeSet

exprSem (Expr_Prop expr (LowerIdent (_, propName))) = do
    exee <- exprSem expr
    cid <- newCid
    let
        objCid = fooCid -- (expCid exee)
        exeGet = mkExePt $ exeProp $ \prop objPt -> do
            execPt $ propGetter prop objPt
        exeSet valPt = mkExe $ exeProp $ \prop objPt -> do
            exec $ propSetter prop objPt valPt
        exeProp :: (PropImpl -> Pointer -> SemiCont a) -> SemiCont a
        exeProp handler k_ =  do
            rValuePt exee $ \objPt re mem -> let
                struct = objStruct (memGet mem objPt) re
                impl = (structClasses struct) !!! objCid
                prop = impl !!! propName
                in (handler prop objPt) k_ re mem
        in return $ LValue cid exeGet exeSet

exprSem (Expr_FunCall expr args) = do
    exeFn <- exprSem expr
    argTuples <- mapM argSem args
    kwargs <- mapM unJustKwarg $ dropWhile fstIsNothing argTuples
    let kwargsSet = foldl (\s (n, _) -> S.insert n s) S.empty kwargs
    when ((S.size kwargsSet) /= (length kwargs)) $ do
        throwError $ SErr "Kwarg names not unique"
    cid <- newCid
    return $ RValue cid $ mkExePt $ \kpt -> do
        rValue exeFn $ \(ValFunction fnImpl) -> do
            execPt (funBody fnImpl argTuples) kpt
    where
        argSem (FunCallArg_Positional argExpr) = do
            exeArg <- exprSem argExpr
            return (Nothing, mkExe $ rValuePt exeArg)
        argSem (FunCallArg_Keyword (LowerIdent (_, name)) argExpr) = do
            exeArg <- exprSem argExpr
            return (Just name, mkExe $ rValuePt exeArg)
        fstIsNothing = (== Nothing).fst
        unJustKwarg (Just name, exe) = return (name, exe)
        unJustKwarg (Nothing, _) = do
            throwError $ SErr "Positional arg after kwargs"

exprSem expr = notYet expr


fnSignatureSem :: FnSignature -> Semantic (FunSgn, M.Map VarName (Exe Pointer))
fnSignatureSem (FnSignature_ args _) = do
    argTuples <- mapM argSem args
    -- TODO: process ret type
    -- OptResultType_None. OptResultType ::= ;
    -- OptResultType_Some. OptResultType ::= "->" TypeExpr;
    -- TODO: check that args have unique names
    let funSgn = FunSgn {
        mthRetType=Nothing,
        mthArgs=(map fst argTuples)
    }
    let defArgsMap = foldl argToMap M.empty argTuples
    return (funSgn, defArgsMap)
    where
        argSem :: FunDef_Arg -> Semantic (ArgSgn, Maybe (Exe Pointer))
        argSem (FunDef_Arg_ _ (LowerIdent (pos, "_")) defVal) = do
            case defVal of
                MaybeDefaultVal_Some _ -> do
                    throwError $ SErrP pos ("Unnamed arguments cannot have " ++
                                            "default value specified (may " ++
                                            "change in future)")
                MaybeDefaultVal_None -> return ()
            -- TODO: process type
            return $ (ArgSgn {
                argName=Nothing,
                argType=undefined,
                argHasDefault=False
            }, Nothing)
        argSem (FunDef_Arg_ _ (LowerIdent (_, name)) defVal) = do
            -- TODO: process type
            (hasDef, defExe) <- case defVal of
                MaybeDefaultVal_None -> return (False, Nothing)
                MaybeDefaultVal_Some expr -> do
                    exee <- exprSem expr
                    let exe = mkExe $ \kpt -> rValue exee $ \val re mem -> do
                        let (pt, mem') = alloc val mem
                        kpt pt re mem'
                    return (True, Just exe)
            return $ (ArgSgn {
                argName=Just name,
                argType=undefined,
                argHasDefault=hasDef
            }, defExe)
        argToMap m (_, Nothing) = m
        argToMap m (ArgSgn{argName=Just name}, Just exe) = M.insert name exe m
        argToMap _ _ = error "argToMap: default value for unnamed argument"


intBinopSem :: (Int -> Int -> Int) -> Expr -> Expr -> Semantic ExprSem
intBinopSem op exp1 exp2 = do
    e1exe <- exprSem exp1
    e2exe <- exprSem exp2
    return $ RValue intCid $ mkExeV $ \ke -> do
        rValue e1exe $ \(ValInt v1) -> do
            rValue e2exe $ \(ValInt v2) -> do
                ke $ ValInt $ op v1 v2

notYet :: Show a => a -> Semantic b
notYet = throwError.(SErr).("not yet: " ++).show
