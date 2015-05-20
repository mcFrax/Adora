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

import ExprExe
import Memory
import Types
import StdLib


data SemState = SemState {
    sstGlob :: GlobEnv,
    sstStructTmpls :: TemplateMap STid StructDesc,
    sstClassTmpls :: TemplateMap CTid ClassDesc,
    sstFunClasses :: M.Map () Cid,
    sstNextCid :: Cid,
    sstNextSid :: Sid,
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

newSid :: Semantic Sid
newSid = do
    stt <- get
    let sid = sstNextSid stt
    put stt{sstNextSid=sid+1}
    return sid


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

showPos :: String -> (Int, Int) -> String
showPos path (ln, col) = path ++ ":" ++ (show ln) ++ ":" ++ (show col)

showSemError :: String -> SErr -> String
showSemError path (SErr s) = path ++ ":?:?: error: " ++ s
showSemError path (SErrP pos s) = do
    (showPos path pos) ++ ": error: " ++ s

stdClss :: M.Map String Cid
stdClss = M.fromList [
        ("Object", 0-0),
        ("Bool", 0-1),
        ("Int", 0-2),
        ("Double", 0-3),
        ("Char", 0-4),
        ("Type", 0-5),
        ("Void", 0-100)
    ]

mkMth :: (Pointer -> FunImpl) -> PropImpl
mkMth mth = PropImpl {
    propGetter=(\pt -> mkExeV $ ($ ValFunction $ mth pt)),
    propSetter=(error "Methods are read-only properties")
}

moduleSem :: Module -> Either SErr (IO ())
moduleSem (Module_ stmts) = do
    let Module_ stdlibStmts = stdlib
    (sst0, env0, exe0) <- do
        case runSemantic (stmtSeqSem stdlibStmts) initSemState initEnv of
            Right res -> return res
            Left errmsg -> do
                error ("Error in standard library:\n" ++
                       (showSemError "stdlib.adora" errmsg))
    (sst, _, exe1) <- runSemantic (stmtSeqSem stmts) sst0 env0
    let
        runEnv = RunEnv {
            reStructs=globStructs $ sstGlob sst,
            reReturn=undefined,
            reBreak=undefined,
            reContinue=undefined
        }
        exe = mkExe $ \ku -> exec exe0 $ \_ -> exec exe1 ku
    return $ runExe exe runEnv initMem
    where
        initSemState = SemState {
            sstGlob=GlobEnv {
                globClasses=M.empty,
                globStructs=M.empty
            },
            sstStructTmpls=(M.empty, M.empty),
            sstClassTmpls=(M.empty, M.empty),
            sstFunClasses=M.empty,
            sstNextCid=0,
            sstNextSid=0,
            sstClassRules=[]
        }

        initEnv = LocEnv {
            envSuper=Nothing,
            envVars=M.fromList [
                ("true", VarType True (stdClss M.! "Bool") Nothing),
                ("false", VarType False (stdClss M.! "Bool") Nothing)
            ],
            envClasses=stdClss,
            envStructs=M.empty,
            envExpectedReturnType=Nothing,
            envInsideLoop=False
        }

        initMem = foldl (flip ($)) emptyMem $ [
                allocVar "true" $ ValBool True,
                allocVar "false" $ ValBool False
            ]

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
stmtBlockSem (StatementBlock_ stmts) = local id $ stmtSeqSem stmts

stmtSeqSem :: [Stmt] -> Semantic (Exe ())
stmtSeqSem stmts = do
    hoisted stmts $ stmtSeqSem' stmts
    where
        stmtSeqSem' [] = return noop -- TODO: check return
        stmtSeqSem' (h:t) = do
            (hexe, t') <- case h of
                Stmt_If (Tok_If (_pos, _)) condExpr bodyBlock -> do
                    let (elses, t') = stripElses t
                    hexe <- ifSem condExpr bodyBlock elses
                    return (hexe, t')
                _ -> do
                    hexe <- stmtSem h
                    return (hexe, t)
            modifiedEnv <- ask  -- TODO: env modification (including var initialization)
            texe <- local (const modifiedEnv) $ stmtSeqSem' t'
            return $ mkExe $ (exec hexe).const.(exec texe)
        stripElses ((Stmt_Elif (Tok_Elif (_pos, _)) condExpr bodyBlock):t) = let
            ((elifs, maybeElse), t') = stripElses t
            in (((condExpr, bodyBlock):elifs, maybeElse), t')
        stripElses ((Stmt_Else (Tok_Else (_pos, _)) bodyBlock):t) = do
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

hoisted :: [Stmt] -> Semantic (Exe a) -> Semantic (Exe a)
hoisted stmts innerSem = do
    compileHoisted <- foldl foldStmt (return $ return ()) stmts
    compileHoisted
    innerSem
    where
        foldStmt hoistPrev stmt = do
            intermediate <- hoistPrev
            hoistLocStmt intermediate stmt
        hoistLocStmt :: Semantic () -> Stmt -> Semantic (Semantic ())
        hoistLocStmt compilePrev (Stmt_Let ident _expr) = do
            hoistVar False ident compilePrev
        hoistLocStmt _compilePrev (Stmt_LetTuple {}) = do
            notYet $ "hoistLocStmt (Stmt_LetTuple ..)"
        hoistLocStmt compilePrev (Stmt_Var ident _expr) = do
            hoistVar True ident compilePrev
        hoistLocStmt compilePrev (Stmt_Decl (TypeAliasDefinition ident typeExpr)) = do
            let UpperIdent (pos, aliasName) = ident
            classes <- asks envClasses
            structs <- asks envStructs
            typeSem <- typeExprSem typeExpr
            (clsFound, classes') <- case expCls typeSem of  -- TODO: a co jak ta druga jest zdefiniowana później?
                Just (Left cid) -> do
                    when (isJust $ M.lookup aliasName classes) $
                        throwError $ SErrP pos $ (
                            "Class `" ++ aliasName ++ "' already exists")
                    return (True, M.insert aliasName cid classes)
                Just (Right _ctid) -> do
                    notYet "Just (Right ctid)"
                Nothing -> return (False, classes)
            (strFound, structs') <- case expStr typeSem of
                Just (Left sid) -> do
                    when (isJust $ M.lookup aliasName structs) $
                        throwError $ SErrP pos $ (
                            "Struct `" ++ aliasName ++ "' already exists")
                    return (True, M.insert aliasName sid structs)
                Just (Right _stid) -> do
                    notYet "Just (Right stid)"
                Nothing -> return (False, structs)
            when (not $ clsFound || strFound) $ do
                error "not $ clsFound || strFound"
            modifyEnv $ \env -> env {
                    envClasses=classes',
                    envStructs=structs'
                }
            return compilePrev

        hoistLocStmt compilePrev (Stmt_Decl clsDef@(TypeDefinition_Class {})) = do
            let (TypeDefinition_Class ident mTmplSgn superClsExprs _variants _decls) = clsDef
            let UpperIdent (pos, clsName) = ident
            classes <- asks envClasses
            when (isJust $ M.lookup clsName classes) $
                throwError $ SErrP pos ("Class `" ++ clsName ++ "' already defined")
            case mTmplSgn of
                MaybeTemplateSgn_None -> do
                    return ()  -- TODO?
                MaybeTemplateSgn_Some _params -> notYet "MaybeTemplateSgn_Some"
            _superClses <- mapM (\(SuperType_ t) -> typeExprSem t) superClsExprs
            cid <- newCid
            modifyEnv $ \env -> env {
                    envClasses=M.insert clsName cid classes
                }
            return compilePrev
        hoistLocStmt compilePrev (Stmt_Decl strDef@(TypeDefinition_Struct {})) = do
            let (TypeDefinition_Struct ident mTmplSgn superStrExprs decls) = strDef
            let UpperIdent (pos, strName) = ident
            beforeClasses <- asks envClasses
            beforeStructs <- asks envStructs
            when (isJust $ M.lookup strName beforeClasses) $
                throwError $ SErrP pos ("Class `" ++ strName ++ "' already defined")
            when (isJust $ M.lookup strName beforeStructs) $
                throwError $ SErrP pos ("Struct `" ++ strName ++ "' already defined")
            case mTmplSgn of
                MaybeTemplateSgn_None -> do
                    return ()  -- TODO?
                MaybeTemplateSgn_Some _params -> notYet "MaybeTemplateSgn_Some"
            _superStrs <- mapM (\(SuperType_ t) -> typeExprSem t) superStrExprs
            cid <- newCid
            sid <- newSid
            let compile = do
                compilePrev
                glob <- gets sstGlob
                attrs <- hoistAttrs
                impls <- hoistImpls
                let
                    attrsMap = M.fromList attrs
                    struct = StructDesc {
                        structName=strName,
                        structCid=cid,
                        structAttrs=attrsMap,
                        structClasses=impls,
                        structCtor=FunImpl $ constructor
                    }
                    constructor argTuples = do
                        mkExePt $ \kpt re mem -> let
                            exe = mkCall ctorSgn M.empty exeCtorBody (0-1) argTuples
                            in exec exe (\(ValInt pt) -> kpt pt) re mem
                    ctorSgn = FunSgn {
                        mthRetType=Just cid,
                        mthArgs=map attrToArg $ attrs
                    }
                    attrToArg (attrName, attrCid) = ArgSgn {
                        argName=Just attrName,
                        argType=attrCid,
                        argHasDefault=False  -- TODO?
                    }
                    exeCtorBody = mkExe $ \_ re mem -> let
                        vars = frameContent $ memFrame mem
                        mem' = allocVar "self" ValObject {
                                valObjStruct=sid,
                                valObjAttrs=M.intersection vars attrsMap
                            } mem
                        -- TODO: execute custom contructor body/init method?
                        pt = (getVarPt "self" mem')
                        in (reReturn re) (ValInt pt) re mem'
                    glob' = glob{
                        globStructs=M.insert sid struct $ globStructs glob
                    }
                    in modify $ \sst -> sst{sstGlob=glob'}
                where
                    hoistAttrs = do
                        attrs <- sequence $ do
                            decl <- decls
                            case decl of
                                (FieldDefinition typeExpr
                                                (LowerIdent (pos', name))
                                                _maybeDefault) -> return $ do
                                    typeSem <- typeExprSem typeExpr
                                    cid' <- case expCls typeSem of
                                        Just (Left cid') -> return cid'
                                        _ -> throwError $ SErrP pos' (
                                                "typeExpr without cid?")
                                    return (name, cid')
                                _ -> []
                        -- TODO check integrity
                        return attrs
                    hoistImpls = return M.empty
            modifyEnv $ \env -> env {
                    envStructs=M.insert strName sid beforeStructs,
                    envClasses=M.insert strName cid beforeClasses
                }
            return compile
        hoistLocStmt _compilePrev (Stmt_Decl _) = do
            error "FOooo" --TODO?
            -- _.                          Decl0 ::= Decl3 ; -- MethodDeclaration
            -- MethodDefinition.           Decl0 ::= Decl3 StatementBlock ;
            -- ImplementationDefinition.   Decl0 ::= "implement" Expr9 ":" "{" [Decl10] "}" ;
            -- FieldDefinition.            Decl0 ::= Expr9 LowerIdent MaybeDefaultVal ;
            -- PropertyDeclaration.        Decl0 ::= "pro" LowerIdent ":" "{" [PropDeclClause] "}" ;
            -- TypeAliasDefinition.  Decl2 ::= "type" UpperIdent "=" Expr9 ;
            -- MethodDeclaration. Decl3 ::= "mth" LowerIdent MaybeTemplateSgn FnSignature ;
            -- TypeDefinition_Class.        Decl7 ::= "class" UpperIdent MaybeTemplateSgn "(" [Expr9] ")" ":" "{" [VariantDefinition] [Decl10] "}" ;
            -- TypeDefinition_ClassStruct.  Decl7 ::= "class" "struct" UpperIdent MaybeTemplateSgn "(" [Expr9] ")" ":" "{" [Decl10] "}" ;
            -- TypeDefinition_Struct.       Decl7 ::= "struct" UpperIdent MaybeTemplateSgn "(" [Expr9] ")" ":" "{" [Decl10] "}" ;
        hoistLocStmt compilePrev _stmt = return compilePrev

        hoistVar :: Bool -> LowerIdent -> Semantic () -> Semantic (Semantic ())
        hoistVar mutable (LowerIdent (pos, varName)) compilePrev = do
            cid <- newCid
            vars <- asks envVars
            case M.lookup varName vars of
                Just var -> do
                    let msg = "variable redefined: `" ++ varName ++ "'"
                    throwError $ SErrP pos $ case varDefPos var of
                        Just pos' -> do
                            msg ++ "'\nPreviously defined at " ++ (showPos "" pos')
                        Nothing -> msg
                Nothing -> do
                    let var = VarType {
                        varClass=cid,
                        varMutable=mutable,
                        varDefPos=Just pos
                    }
                    modifyEnv $ \env -> env{envVars=M.insert varName var vars}
                    return compilePrev



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                  __            __  _____                                   --
--            _____/ /_____ ___  / /_/ ___/___  ____ ___                      --
--           / ___/ __/ __ `__ \/ __/\__ \/ _ \/ __ `__ \                     --
--          (__  ) /_/ / / / / / /_ ___/ /  __/ / / / / /                     --
--         /____/\__/_/ /_/ /_/\__//____/\___/_/ /_/ /_/                      --
--                                                                            --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


stmtSem :: Stmt -> Semantic (Exe ())
stmtSem (Stmt_Decl _) = return noop

stmtSem (Stmt_Expr expr) = do
    esem <- exprSem expr
    return $ mkExe $ \k -> do
        rValue esem $ \_ -> k ()
--         rValue esem $ \v -> do
--             io (hPutStrLn stderr $ "Stmt_Expr: " ++ (show v)) k

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
stmtSem (Stmt_Elif (Tok_Elif (pos, _)) _ _) = do
    throwError $ SErrP pos "Unexpected elif statement"
stmtSem (Stmt_Else (Tok_Else (pos, _)) _) = do
    throwError $ SErrP pos "Unexpected else statement"

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
        Just _cid ->
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
        Just _cid -> do
            -- TODO: check cid
            eexe <- exprSem expr
            return $ mkExe $ \_ -> rValue eexe $ \val re -> reReturn re val re
        Nothing ->
            throwError $ SErrP pos ("Unexpected return statement " ++
                                    "outside of function body")

stmtSem (Stmt_Assert (Tok_Assert (pos, _)) expr) = do
    eexe <- exprSem expr
    return $ mkExe $ \k -> rValue eexe $ \val -> do
        if isTruthy val then
            k ()
        else
            \_ _ -> do
                hPutStrLn stderr ("Assertion at " ++ (showPos "" pos) ++
                                  "failed: " ++ (printTree expr))
                exitFailure

stmtSem (Stmt_Print {}) = do
    return $ mkExe $ \k re mem -> do
        putChar '\n'
        k () re mem

stmtSem (Stmt_PrintValues (Tok_Print (_pos, _)) exprs) = do
    exprExes <- mapM exprSem exprs
    return $ doPrint exprExes
    where
        doPrint [exprExe] = mkExe $ \k -> do
            rValue exprExe $ \val re mem -> do
                putStr (show val)
                putChar '\n'
                k () re mem
        doPrint (exprExe:exprExes) = mkExe $ \k -> do
            rValue exprExe $ \val re mem -> do
                putStr (show val)
                putChar ' '
                exec (doPrint exprExes) k re mem
        doPrint [] = undefined

-- Stmt_LetTuple.      Stmt ::= "let" "(" [LowerIdent] ")" "=" Expr ;  -- tuple unpacking
-- Stmt_Case.          Stmt ::= "case" Expr "class" "of" "{" [CaseClause] "}";
-- Stmt_ForIn.         Stmt ::= "for" LowerIdent "in" Expr StatementBlock ;
-- Stmt_For.           Stmt ::= "for" LowerIdent "=" Expr "then" Expr StatementBlock ;
-- Stmt_ForWhile.      Stmt ::= "for" LowerIdent "=" Expr "then" Expr "while" Expr StatementBlock ;
stmtSem stmt = notYet stmt



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                                 _____                                      --
--           ___  _  ______  _____/ ___/___  ____ ___                         --
--          / _ \| |/_/ __ \/ ___/\__ \/ _ \/ __ `__ \                        --
--         /  __/>  </ /_/ / /   ___/ /  __/ / / / / /                        --
--         \___/_/|_/ .___/_/   /____/\___/_/ /_/ /_/                         --
--                 /_/                                                        --
--                                                                            --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


typeExprSem :: Expr -> Semantic ExprSem
typeExprSem typeExpr = do
    sem <- exprSem typeExpr
    case sem of
        TypeValue {} -> return sem
        _ -> throwError $ SErr{-P pos-} ("`" ++ (printTree typeExpr) ++
                                         "' is not a proper type name")

exprSem :: Expr -> Semantic ExprSem
exprSem (Expr_Char c) = return $ RValue (stdClss M.! "Char") $ mkExeV ($ ValChar c)
-- exprSem (Expr_String c) _ =
-- exprSem (Expr_Double d) _ = liftM RValue $ return $ ValDouble d
exprSem (Expr_Int i) = return $ RValue (stdClss M.! "Int") $ mkExeV ($ ValInt $ fromInteger i)
-- exprSem (Expr_Tuple _) _ = ???
-- exprSem (Expr_Array _) _ = ???
exprSem (Expr_Var (LowerIdent (_, varName))) = do
    -- TODO: check env
    let cid = (stdClss M.! "Int") -- TODO TODO TODO
        exeGet = mkExePt $ \kpt re mem -> kpt (getVarPt varName mem) re mem
        exeSet pt = mkExe $ \k re mem -> k () re (assignFrameVar varName pt mem)
    return $ LValue cid exeGet exeSet

exprSem (Expr_Not expr) = do
    exee <- exprSem expr
    return $ RValue (stdClss M.! "Bool") $ mkExeV $ \ke -> do
        rValue exee $ \(ValBool bval) -> do
            ke $ ValBool $ not bval

exprSem expr@(Expr_RelOper {}) = do
    sem' <- exprSem' expr
    return $ RValue (stdClss M.! "Bool") $ mkExeV $ \ke -> do
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
--     return $ RValue (stdClss M.! "Bool") $ mkExeV $ \ke -> do
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
    let lambdaVal closureFid = do
        ValFunction $ FunImpl $ (Left.) $ mkCall fnSgn defArgs exeBody closureFid
    return $ RValue cid $ mkExeV $ \ke re mem -> do
        ke (lambdaVal $ memFid mem) re mem

exprSem (Expr_TypeName (UpperIdent (pos, typeName))) = do
    maybeSid <- asks $ (M.lookup typeName).envStructs
    maybeCid <- asks $ (M.lookup typeName).envClasses
    when (not $ any isJust [maybeSid, maybeCid]) $ do
        throwError $ SErrP pos ("Undefined class name: `" ++ typeName ++ "'")
    rvalExe <- case maybeSid of
        Just sid -> do
            struct <- gets $ (!!! sid).globStructs.sstGlob
            let _strCid = structCid struct
            return $ mkExeV $ \ke -> ke $ ValFunction $ structCtor struct
        _ -> return $ mkExeV ($ ValNull)
    cid <- newCid  -- cid ==> ((??) -> <struct with cid=strCid>)
    return $ TypeValue {
        expCid=cid,
        expRValue=rvalExe,
        expCls=maybeCid >>= (return.Left),
        expStr=maybeSid >>= (return.Left)
    }

exprSem (Expr_Field expr (LowerIdent (_, attrName))) = do
    exee <- exprSem expr
    let cid = (stdClss M.! "Int") -- cid <- newCid
    let
        _objCid = expCid exee
        exeGet = mkExePt $ exeAttr $ \_objPt attrs kpt re mem -> do
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
        _objCid = expCid exee
        exeGet = mkExePt $ exeProp $ \prop objPt -> do
            execPt $ propGetter prop objPt
        exeSet valPt = mkExe $ exeProp $ \prop objPt -> do
            exec $ propSetter prop objPt valPt
        exeProp :: (PropImpl -> Pointer -> SemiCont a) -> SemiCont a
        exeProp handler k_ =  do
            rValuePt exee $ \objPt re mem -> let
                struct = objStruct (memGet mem objPt) re
                impl = (structClasses struct) !!! (structCid struct) -- objCid
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

exprSem (Expr_Parens [expr]) = exprSem expr

-- exprSem (Expr_Parens exprs) = {- tuple -}

exprSem expr = notYet expr



fnSignatureSem :: FnSignature -> Semantic (FunSgn, M.Map VarName (Exe Pointer))
fnSignatureSem (FnSignature_ args optResType) = do
    argTuples <- mapM argSem args
    retType <- case optResType of
        OptResultType_None -> return Nothing
        OptResultType_Some typeExpr -> do
            typeSem <- typeExprSem typeExpr
            case expCls typeSem of
                Just (Left retCid) -> return $ Just retCid
                _ -> throwError $ SErr ("typeExpr without cid?")
    let funSgn = FunSgn {
        mthRetType=retType,
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

mkCall :: FunSgn -> M.Map VarName (Exe Pointer) -> Exe () -> Fid ->
    [(Maybe VarName, Exe Pointer)] -> Exe Value
mkCall fnSgn defArgs exeBody closureFid argTuples = do
    mkExe $ \ke re0 mem0 -> let
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
            (exec exeBody $ const $ doReturn ValNull) re $ mem{memFid=fid}

        in (exec exeArgs $ const $ exec exeDefArgs $ const bodyCont) re1 mem1
        -- TODO: statycznie wymusic wywolanie return w funkcji, "prawdziwe" lub sztuczne


intBinopSem :: (Int -> Int -> Int) -> Expr -> Expr -> Semantic ExprSem
intBinopSem op exp1 exp2 = do
    e1exe <- exprSem exp1
    e2exe <- exprSem exp2
    return $ RValue (stdClss M.! "Int") $ mkExeV $ \ke -> do
        rValue e1exe $ \(ValInt v1) -> do
            rValue e2exe $ \(ValInt v2) -> do
                ke $ ValInt $ op v1 v2

notYet :: Show a => a -> Semantic b
notYet = throwError.(SErr).("not yet: " ++).show
