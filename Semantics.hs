{-# OPTIONS -XMultiParamTypeClasses #-}

module Semantics where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

import System.IO

import Absadora
import Printadora(printTree)

import Memory
import Types
import StdLib


data SemState = SemState {
    sstGlob :: GlobEnv,
    sstStructTmpls :: TemplateMap STid StructDesc,
    sstClassTmpls :: TemplateMap CTid ClassDesc,
    sstFunClasses :: M.Map FunSgn Cid,
    sstNextCid :: Cid,
    sstNextSid :: Sid,
    sstClassRules :: [ClassRule],
    sstFileName :: String
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
    modify $ \sst -> sst{sstClassRules=rule:(sstClassRules sst)}

newCid :: Semantic Cid
newCid = do
    sst <- get
    let cid@(Cid cidVal) = sstNextCid sst
    put sst{sstNextCid=Cid $ cidVal + 1}
    return cid

fakeCid :: Semantic Cid
fakeCid = newCid

newSid :: Semantic Sid
newSid = do
    sst <- get
    let sid@(Sid sidVal) = sstNextSid sst
    put sst{sstNextSid=Sid $ sidVal + 1}
    return sid

getFunctionCid :: FunSgn -> Semantic Cid
getFunctionCid fnSgn = do
    funClasses <- gets sstFunClasses
    case M.lookup fnSgn funClasses of
        Just cid -> return cid
        Nothing -> do
            cid <- newCid
            glob <- gets sstGlob
            let funClasses' = M.insert fnSgn cid funClasses
                classes' = M.insert cid (ClassFun fnSgn) $ globClasses glob
            modify $ \sst -> sst{
                    sstGlob=glob{globClasses=classes'},
                    sstFunClasses=funClasses'
                }
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
          | SErrP CodePosition String

instance Error SErr where
  strMsg = SErr

showSemError :: SErr -> String
showSemError (SErr s) = "?:?:?: error: " ++ s
showSemError (SErrP pos s) = do
    (showPos pos) ++ ": error: " ++ s

showPos :: CodePosition -> String
showPos (path, ln, col) = path ++ ":" ++ (show ln) ++ ":" ++ (show col)

completePos :: (Int, Int) -> Semantic CodePosition
completePos (ln, col) = do
    fileName <- gets sstFileName
    return (fileName, ln, col)

stdClss :: M.Map String Cid
stdClss = M.fromList $ map (\(n, cv) -> (n, Cid cv)) [
        ("Object", -0),
        ("Bool", -1),
        ("Int", -2),
        ("Double", -3),
        ("Char", -4),
        ("Type", -5),
        ("Void", -100)
    ]

stdGlobClss :: CidMap
stdGlobClss = do
    M.fromList $ map f $ M.toList stdClss
    where
        f (name, cid) = (cid, ClassDesc {
            className_=name,
            classProps_=M.empty
        })

topLevelFid :: Fid
topLevelFid = Fid 0


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                                 __      __    _____                        --
--            ____ ___  ____  ____/ /_  __/ /__ / ___/___  ____ ___           --
--           / __ `__ \/ __ \/ __  / / / / / _ \\__ \/ _ \/ __ `__ \          --
--          / / / / / / /_/ / /_/ / /_/ / /  __/__/ /  __/ / / / / /          --
--         /_/ /_/ /_/\____/\__,_/\__,_/_/\___/____/\___/_/ /_/ /_/           --
--                                                                            --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

moduleSem :: Module -> String -> Either SErr (IO ())
moduleSem (Module_ stmts) fileName = do
    let Module_ stdlibStmts = stdlib
    (sst0, env0, exe0) <- do
        case runSemantic (stmtSeqSem stdlibStmts) initSemState initEnv of
            Right res -> return res
            Left errmsg -> do
                error ("Error in standard library:\n" ++
                       (showSemError errmsg))
    (sst, _, exe1) <- runSemantic (stmtSeqSem stmts) sst0{sstFileName=fileName} env0
    let
        runEnv = RunEnv {
            reStructs=globStructs $ sstGlob sst,
            reReturn=error "return outside of function",
            reBreak=error "break outside a loop",
            reContinue=error "continue outside a loop"
        }
        exe = mkExe $ \ku -> exec exe0 $ \_ -> exec exe1 ku
    return $ runExe exe runEnv initMem
    where
        initSemState = SemState {
            sstGlob=GlobEnv {
                globClasses=stdGlobClss,
                globStructs=M.empty
            },
            sstStructTmpls=(M.empty, M.empty),
            sstClassTmpls=(M.empty, M.empty),
            sstFunClasses=M.empty,
            sstNextCid=Cid 0,
            sstNextSid=Sid 0,
            sstClassRules=[],
            sstFileName="<stdlib>"
        }

        initEnv = LocEnv {
            envSuper=Nothing,
            envVars=M.fromList [
                ("true", VarType True (stdClss M.! "Bool") ("<internal>", 0, 0)),
                ("false", VarType False (stdClss M.! "Bool") ("<internal>", 0, 0))
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
            memFid=topLevelFid,
            memObjects=M.empty,
            memFrames=M.fromList [(topLevelFid, globFrame)]
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
                exec (expRValue exeCond) $ \condVal ->
                    if isTruthy condVal then
                        exec exeThen k
                    else
                        exec exeElse k
            where
                elseSem ([], Nothing) = return noop
                elseSem ([], Just elseBody) = stmtBlockSem elseBody
                elseSem (((condExpr', bodyBlock'):elifs), maybeElse) = do
                    ifSem condExpr' bodyBlock' (elifs, maybeElse)



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                     __          _      __           __                     --
--                    / /_  ____  (_)____/ /____  ____/ /                     --
--                   / __ \/ __ \/ / ___/ __/ _ \/ __  /                      --
--                  / / / / /_/ / (__  ) /_/  __/ /_/ /                       --
--               __/_/ /_/\____/_/____/\__/\___/___,_/                        --
--          ____/ /__  _____/ /___ __________ _/ /_(_)___  ____  _____        --
--         / __  / _ \/ ___/ / __ `/ ___/ __ `/ __/ / __ \/ __ \/ ___/        --
--        / /_/ /  __/ /__/ / /_/ / /  / /_/ / /_/ / /_/ / / / (__  )         --
--        \__,_/\___/\___/_/\__,_/_/   \__,_/\__/_/\____/_/ /_/____/          --
--                                                                            --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


data HoistingLevel = HoistingLevel (Semantic HoistingLevel)
                   | HoistingEnd

hoisted :: [Stmt] -> Semantic (Exe a) -> Semantic (Exe a)
hoisted stmts innerSem = do
    level1 <- (foldl (>>=) (return HoistingEnd)) =<< (mapM hoistStmt stmts)
    hoist level1
    innerSem
    where
        hoist (HoistingLevel nextLevel) = do
            nextLevel' <- nextLevel
            hoist nextLevel'
        hoist HoistingEnd = return ()

queueNextLevel :: Semantic (HoistingLevel -> Semantic HoistingLevel)
    -> Semantic (HoistingLevel -> Semantic HoistingLevel)
queueNextLevel nextLevel = do
    nextLevel' <- nextLevel
    return $ \hoistingAcc -> do
        case hoistingAcc of
            HoistingLevel hoistingAcc' -> return $ HoistingLevel $ do
                hoistingAcc' >>= nextLevel'
            HoistingEnd -> return $ HoistingLevel $ nextLevel' HoistingEnd

queueNextLevel2 :: Semantic (HoistingLevel -> Semantic HoistingLevel)
    -> Semantic (HoistingLevel -> Semantic HoistingLevel)
queueNextLevel2 = queueNextLevel.queueNextLevel

hoistingDone :: Semantic (HoistingLevel -> Semantic HoistingLevel)
hoistingDone = return $ return

hoistStmt :: Stmt -> Semantic (HoistingLevel -> Semantic HoistingLevel)
hoistStmt (Stmt_Let ident _ expr) = do
    cid <- deduceType expr
    hoistVar cid False ident
hoistStmt (Stmt_LetTuple {}) = do
    notYet $ "hoistStmt (Stmt_LetTuple ..)"
hoistStmt (Stmt_Var ident _ expr) = do
    cid <- deduceType expr
    hoistVar cid True ident
hoistStmt (Stmt_Decl (TypeAliasDefinition _ident _ _typeExpr)) = do
    --TODO
--     let UpperIdent (pos, aliasName) = ident
--     classes <- asks envClasses
--     structs <- asks envStructs
--     typeSem <- typeExprSem typeExpr
--     (clsFound, classes') <- case expCls typeSem of
--         Just (Left cid) -> do
--             when (aliasName `M.member` classes) $
--                 throwAt pos $ (
--                     "Class `" ++ aliasName ++ "' already exists")
--             return (True, M.insert aliasName cid classes)
--         Just (Right _ctid) -> do
--             notYetAt pos "Just (Right ctid)"
--         Nothing -> return (False, classes)
--     (strFound, structs') <- case expStr typeSem of
--         Just (Left sid) -> do
--             when (aliasName `M.member` structs) $
--                 throwAt pos $ (
--                     "Struct `" ++ aliasName ++ "' already exists")
--             return (True, M.insert aliasName sid structs)
--         Just (Right _stid) -> do
--             notYetAt pos "Just (Right stid)"
--         Nothing -> return (False, structs)
--     when (not $ clsFound || strFound) $ do
--         error "not $ clsFound || strFound"
--     modifyEnv $ \env -> env {
--             envClasses=classes',
--             envStructs=structs'
--         }
--     queueNextLevel $ do
    hoistingDone

hoistStmt (Stmt_Decl clsDef@(TypeDefinition_Class {})) = do
    let (TypeDefinition_Class ident mTmplSgn _ superClsExprs variants decls) = clsDef
    let UpperIdent (pos, clsName) = ident
    beforeClasses <- asks envClasses
    when (clsName `M.member` beforeClasses) $
        throwAt pos ("Class `" ++ clsName ++ "' already defined")
    assertTmplSgnEmpty mTmplSgn
    _superClses <- mapM (\(SuperType_ t) -> typeExprSem t) superClsExprs
    cid <- newCid
    modifyEnv $ \env -> env {
            envClasses=M.insert clsName cid beforeClasses
        }
    queueNextLevel2 $ do
        cls <- compileClass cid ident mTmplSgn superClsExprs variants decls hoistClsDecl
        glob <- gets sstGlob
        let glob' = glob{
                globClasses=M.insert cid cls $ globClasses glob
            }
        modify $ \sst -> sst{sstGlob=glob'}
        hoistingDone

hoistStmt (Stmt_Decl strDef@(TypeDefinition_Struct {})) = do
    let (TypeDefinition_Struct ident mTmplSgn _ superStrExprs decls) = strDef
    let UpperIdent (strPos, strName) = ident
    beforeClasses <- asks envClasses
    beforeStructs <- asks envStructs
    when (strName `M.member` beforeClasses) $
        throwAt strPos ("Class `" ++ strName ++ "' already defined")
    when (strName `M.member` beforeStructs) $
        throwAt strPos ("Struct `" ++ strName ++ "' already defined")
    assertTmplSgnEmpty mTmplSgn
    _superStrs <- mapM (\(SuperType_ t) -> typeExprSem t) superStrExprs
    sid <- newSid
    cid <- newCid
    modifyEnv $ \env -> env {
            envStructs=M.insert strName sid beforeStructs,
            envClasses=M.insert strName cid beforeClasses
        }
    queueNextLevel2 $ do  -- 2
        cls <- compileClass cid ident mTmplSgn superStrExprs [] decls hoistStrClsDecl
        modify $ \sst -> let
            glob = sstGlob sst
            glob' = glob{
                    globClasses=M.insert cid cls $ globClasses glob
                }
            in sst{sstGlob=glob'}
        queueNextLevel $ do  -- 3
            let initImpls = M.fromList [(cid, M.empty)]
            revAttrs <- foldl (>>=) (return []) $ map (hoistStrAttr cid) decls
            let attrs = reverse revAttrs
                attrsMap = M.fromList attrs
                structStub = StructDesc {
                    structName=strName,
                    structCid=cid,
                    structAttrs=attrsMap,
                    structClasses=error "Undefined structClasses",
                    structCtor=error "Undefined structCtor",
                    structCtorSgn=error "Undefined structCtorSgn"
                }
            modify $ \sst -> let
                glob = sstGlob sst
                glob' = glob{
                        globStructs=M.insert sid structStub $ globStructs glob
                    }
                in sst{sstGlob=glob'}
            queueNextLevel2 $ do  -- 5
                impls <- foldl (>>=) (return initImpls) $ map (hoistStrDecl cid) decls
                let struct = structStub {
                        structClasses=impls,
                        structCtor=FunImpl $ constructor,
                        structCtorSgn=ctorSgn
                    }
                    constructor argTuples = do
                        mkCall ctorSgn M.empty exeCtorBody topLevelFid argTuples
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
                        (pt, mem') = allocObject Object {
                                objSid=sid,
                                objAttrs=M.intersection vars attrsMap
                            } mem
                        -- mem'' = allocVar "self" (ValRef pt) mem' -- local ctor variable
                        -- TODO: execute custom contructor body/init method?
                        in (reReturn re) (ValRef pt) re mem'
                modify $ \sst -> let
                    glob = sstGlob sst
                    glob' = glob{
                            globStructs=M.insert sid struct $ globStructs glob
                        }
                    in sst{sstGlob=glob'}
                hoistingDone

hoistStmt (Stmt_Decl Decl_Pass) = hoistingDone

hoistStmt (Stmt_Decl decl) = do
    notYet $ "Hoisting for `" ++ (printTree decl) ++ "'"

hoistStmt _stmt = hoistingDone


hoistVar :: Cid -> Bool -> LowerIdent -> Semantic (HoistingLevel -> Semantic HoistingLevel)
hoistVar cid mutable (LowerIdent (pos, varName)) = do
    queueNextLevel2 $ queueNextLevel2 $ do
        vars <- asks envVars
        fpos <- completePos pos
        case M.lookup varName vars of
            Just var -> do
                throwAt pos (
                    "variable redefined: `" ++ varName ++ "'\n" ++
                    "Previously defined at " ++ (showPos $ varDefPos var))
            Nothing -> do
                let var = VarType {
                    varClass=cid,
                    varMutable=mutable,
                    varDefPos=fpos
                }
                modifyEnv $ \env -> env{envVars=M.insert varName var vars}
                hoistingDone


hoistStrAttr :: Cid -> Decl -> [(VarName, Cid)]
    -> Semantic [(VarName, Cid)]
hoistStrAttr _ decl@(FieldDefinition {}) attrs = do
    let (FieldDefinition
            typeExpr
            (LowerIdent (_pos, name))
            _maybeDefault) = decl
    cid <- typeExprCid typeExpr
    return $ (name, cid):attrs
hoistStrAttr _ _ attrs = return attrs

hoistStrDecl :: Cid -> Decl -> (M.Map Cid Impl)
    -> Semantic (M.Map Cid Impl)
hoistStrDecl _ (FieldDefinition {}) impls = return impls
hoistStrDecl ownCid (MethodDefinition mthDecl bodyBlock) impls = do
    let (MethodDeclaration
            (LowerIdent (pos, name))
            mTmplSgn
            signature) = mthDecl
    fpos <- completePos pos
    assertTmplSgnEmpty mTmplSgn
    (fnSgn, defArgs, argDefPoss) <- fnSignatureSem signature CompileDefaults
--                     outerVars <- asks envVars
    let argVars = M.insert "self" (VarType {
            varMutable=False,
            varClass=ownCid,
            varDefPos=fpos
        }) $ argsToVars (mthArgs fnSgn) argDefPoss
    let makeInEnv outEnv = outEnv{
            envVars=argVars, -- M.union outerVars argVars,
            envExpectedReturnType=Just $ mthRetType fnSgn,
            envInsideLoop=False
        }
    exeBody <- local makeInEnv $ stmtBlockSem bodyBlock
    let propImpl = mkMth fpos $ \pt -> FunImpl $ \args -> do
            let args' = (Just "self", mkExe $ \kv-> kv $ ValRef pt):args
            mkCall fnSgn defArgs exeBody topLevelFid args'
    addPropToImpls name pos propImpl [ownCid] impls
hoistStrDecl ownCid propDecl@(PropertyDeclaration {}) impls = do
    let (PropertyDeclaration
            (LowerIdent (pos, name))
            typeExpr
            getDef
            maybeSet) = propDecl
    cid <- typeExprCid typeExpr
    fpos <- completePos pos
    getter <- case getDef of
        PropDefClause_None -> do
            throwAt pos ("Getter declaration without " ++
                            "definition in struct definition")
        (PropDefClause_Def bodyBlock) -> do
            let getterSgn = FunSgn {
                mthRetType=Just cid,
                mthArgs=[]
            }
--                          outerVars <- asks envVars
            let makeInEnv outEnv = outEnv{
                    envVars=M.fromList [("self", VarType {
                        varMutable=False,
                        varClass=ownCid,
                        varDefPos=fpos
                    })], -- M.union outerVars argVars,
                    envExpectedReturnType=Just $ Just cid,
                    envInsideLoop=False
                }
            exeBody <- local makeInEnv $ stmtBlockSem bodyBlock
            return $ \pt ->
                mkCall getterSgn M.empty exeBody topLevelFid [
                    (Just "self", mkExe $ \kv-> kv $ ValRef pt)]
        PropDefClause_Auto -> do
            notYetAt pos "custom getter"
    setter <- case maybeSet of
        MaybeSetClause_None -> return $ error "Read-only property"
        MaybeSetClause_Some PropDefClause_None -> do
            throwAt pos ("Setter declaration without " ++
                            "definition in struct definition")
        MaybeSetClause_Some (PropDefClause_Def _bodyBlock) -> do
            notYetAt pos "custom setter"
        MaybeSetClause_Some PropDefClause_Auto -> do
            notYetAt pos "custom setter"
    let propImpl = PropImpl {
        propGetter=getter,
        propSetter=setter,
        propDefPos=fpos
    }
    addPropToImpls name pos propImpl [ownCid] impls
hoistStrDecl _ (ImplementationDefinition {}) _ = do
    notYet "ImplementationDefinition"
hoistStrDecl _ (TypeAliasDefinition {}) _ = do
    notYet "Nested types"
hoistStrDecl _ (TypeDefinition_Class {}) _ = do
    notYet "Nested types"
hoistStrDecl _ (TypeDefinition_Struct {}) _ = do
    notYet "Nested types"
hoistStrDecl _ (MethodDeclaration (LowerIdent (pos, _)) _ _) _ = do
    throwAt pos $ (
        "Method declaration without definition in " ++
        "struct definition")
hoistStrDecl _ Decl_Pass impls = return impls


mkMth :: CodePosition -> (MemPt -> FunImpl) -> PropImpl
mkMth fpos mth = PropImpl {
    propGetter=(\pt -> mkExe ($ ValFunction $ mth pt)),
    propSetter=(error "Methods are read-only properties"),
    propDefPos=fpos
}


compileClass :: Cid -> UpperIdent -> MaybeTemplateSgn -> [SuperType] -> [VariantDefinition]
    -> [Decl] -> HoistClsDeclFun -> Semantic ClassDesc
compileClass cid ident mTmplSgn superClsExprs variants decls hoistDecl = do
    let UpperIdent (clsPos, clsName) = ident
    assertTmplSgnEmpty mTmplSgn
    _superClses <- mapM (\(SuperType_ t) -> typeExprSem t) superClsExprs
    when (not $ null variants) $ notYetAt clsPos "variants"
    props <- foldl (>>=) (return M.empty) $ map (hoistDecl cid) decls
    let cls = ClassDesc {
            className_=clsName,
            classProps_=props
        }
    return cls

type HoistClsDeclFun = Cid -> Decl -> M.Map VarName VarType
    -> Semantic (M.Map VarName VarType)

hoistStrClsDecl :: HoistClsDeclFun
hoistStrClsDecl _ (FieldDefinition {}) props = return props
hoistStrClsDecl ownCid (MethodDefinition mthDecl _bodyBlock) props = do
    let (MethodDeclaration ident mTmplSgn
         (FnSignature_ lp args optResType)) = mthDecl
        mthDecl' = (MethodDeclaration ident mTmplSgn
                    (FnSignature_ lp (map remDef args) optResType))
    hoistClsDecl ownCid mthDecl' props
    where
        remDef (FunDef_Arg_ typeName name _) = do
            FunDef_Arg_ typeName name MaybeDefaultVal_None
hoistStrClsDecl ownCid propDecl@(PropertyDeclaration {}) props = do
    let (PropertyDeclaration
            (LowerIdent (pos, name))
            typeExpr
            _getDef
            maybeSet) = propDecl
    let maybeSet' = case maybeSet of
            MaybeSetClause_None -> MaybeSetClause_None
            _ -> MaybeSetClause_Some PropDefClause_None
    hoistClsDecl ownCid (PropertyDeclaration
            (LowerIdent (pos, name))
            typeExpr
            PropDefClause_None
            maybeSet') props
hoistStrClsDecl _ (ImplementationDefinition {}) props = return props
hoistStrClsDecl _ (TypeAliasDefinition {}) _ = do
    notYet "Nested types"
hoistStrClsDecl _ (TypeDefinition_Class {}) _ = do
    notYet "Nested types"
hoistStrClsDecl _ (TypeDefinition_Struct {}) _ = do
    notYet "Nested types"
hoistStrClsDecl _ (MethodDeclaration {}) _ = do
    error "MethodDeclaration in hoistStrClsDecl"  -- should have thrown before
hoistStrClsDecl _ Decl_Pass props = return props

hoistClsDecl :: HoistClsDeclFun
hoistClsDecl _ (FieldDefinition _ (LowerIdent (pos, _)) _) _ = do
    throwAt pos "Attribute definition inside class (only structs can have attrs)"
hoistClsDecl _ (MethodDefinition mthDecl _) _ = do
    let MethodDeclaration (LowerIdent (pos, _)) _ _ = mthDecl
    throwAt pos "Method definition inside class (only declarations allowed)"
hoistClsDecl _ propDecl@(PropertyDeclaration {}) props = do
    let (PropertyDeclaration
            (LowerIdent (pos, name))
            typeExpr
            getDef
            maybeSet) = propDecl
    when (getDef /= PropDefClause_None) $ do
        throwAt pos "Getter definition in class (only declarations allowed)"
    mutable <- case maybeSet of
        MaybeSetClause_None -> return False
        MaybeSetClause_Some PropDefClause_None -> return True
        _ -> throwAt pos "Setter definition in class (only declarations allowed)"
    propCid <- typeExprCid typeExpr
    fpos <- completePos pos
    let propType = VarType {
            varMutable=mutable,
            varClass=propCid,
            varDefPos=fpos
        }
    addPropToMap name pos propType props
hoistClsDecl _ (ImplementationDefinition {}) props = return props
hoistClsDecl _ (TypeAliasDefinition {}) _ = do
    notYet "Nested types"
hoistClsDecl _ (TypeDefinition_Class {}) _ = do
    notYet "Nested types"
hoistClsDecl _ (TypeDefinition_Struct {}) _ = do
    notYet "Nested types"
hoistClsDecl _ decl@(MethodDeclaration {}) props = do
    let (MethodDeclaration
            (LowerIdent (pos, name))
            mTmplSgn
            signature) = decl
    fpos <- completePos pos
    assertTmplSgnEmpty mTmplSgn
    (fnSgn, _, _) <- fnSignatureSem signature RejectDefaults
    propCid <- getFunctionCid fnSgn
    let propType = VarType {
            varMutable=False,
            varClass=propCid,
            varDefPos=fpos
        }
    addPropToMap name pos propType props
hoistClsDecl _ Decl_Pass props = return props

addPropToImpls :: VarName -> (Int, Int) -> PropImpl -> [Cid] -> M.Map Cid Impl
    -> Semantic (M.Map Cid Impl)
addPropToImpls name pos propImpl cids impls0 = do
    foldl (>>=) (return impls0) (map addProp' cids)
    where
        addProp' cid impls = do
            impl' <- addPropToMap name pos propImpl (impls M.! cid)
            return $ M.insert cid impl' impls

addPropToMap :: VarName -> (Int, Int) -> a -> M.Map VarName a
    -> Semantic (M.Map VarName a)
addPropToMap name pos propImpl propMap = do
    case M.insertLookupWithKey (\_ _ x -> x) name propImpl propMap of
        (Nothing, propMap') -> do
            return propMap'
        (Just _, _) -> do
            throwAt pos (
                "Property or method `" ++ name ++ "' already defined")



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
        execRValue esem $ \_ -> k ()

stmtSem (Stmt_Let (LowerIdent (_, varName)) _ expr) = do
    eexe <- exprSem expr
--     check for shadowing ...
--     modifyEnv ...
    return $ mkExe $ \k -> do
        execRValue eexe $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Var (LowerIdent (_, varName)) _ expr) = do
    eexe <- exprSem expr
--     modifyEnv - var initialized?
    return $ mkExe $ \k -> do
        execRValue eexe $ \val re mem -> do
            k () re $ allocVar varName val mem

stmtSem (Stmt_Assign lexpr (Tok_Assign (pos, _)) rexpr) = do
    lexe <- exprSem lexpr
    rexe <- exprSem rexpr
    case lexe of
        LValue {} -> do
            if ((expCid lexe) == (expCid rexe)) then
                return $ mkExe $ \k -> do
                    execRValue rexe $ \val -> exec (setLValue lexe val) k
            else do
                lcls <- getCls $ expCid lexe
                rcls <- getCls $ expCid rexe
                throwAt pos ("Value of type `" ++ (className lcls) ++ "' expected, `" ++
                             (className rcls) ++ "' found")
        _ -> throwAt pos "Left side of assignment is not assignable"

stmtSem (Stmt_If {}) = error "Stmt_If should have been handled by stmtSeqSem"
stmtSem (Stmt_Elif (Tok_Elif (pos, _)) _ _) = do
    throwAt pos "Unexpected elif statement"
stmtSem (Stmt_Else (Tok_Else (pos, _)) _) = do
    throwAt pos "Unexpected else statement"

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
            let k' = execRValue exeCond $ \condVal _ -> do
                if isTruthy condVal then
                    exec exeBody (\_ -> (exec exeLoop k).popRe) re'
                else
                    k () re
            k' re'
    where
        fix :: (a -> a) -> a
        fix f = f $ fix f

stmtSem (Stmt_Break (Tok_Break (pos, _))) = do
    insideLoop <- asks envInsideLoop
    if insideLoop then
        return $ mkExe $ \_ re -> reBreak re re
    else
        throwAt pos $ "Unexpected break statement"

stmtSem (Stmt_Continue (Tok_Continue (pos, _))) = do
    insideLoop <- asks envInsideLoop
    if insideLoop then
        return $ mkExe $ \_ re -> reContinue re re
    else
        throwAt pos $ "Unexpected continue statement"

stmtSem (Stmt_Return (Tok_Return (pos, _))) = do
    rType <- asks envExpectedReturnType
    case rType of
        Just Nothing ->
            return $ mkExe $ \_ re -> reReturn re ValNull re
        Just _cid ->
            throwAt pos ("Return statement without value " ++
                                    "in funtion with return type")
        Nothing ->
            throwAt pos ("Unexpected return statement " ++
                                    "outside of function body")

stmtSem (Stmt_ReturnValue (Tok_Return (pos, _)) expr) = do
    rType <- asks envExpectedReturnType
    case rType of
        Just Nothing ->
            throwAt pos ("Return statement with value " ++
                                    "in funtion returning nothing")
        Just (Just expectedCid) -> do
            eexe <- exprSem expr
            when ((expCid eexe) /= expectedCid) $ do
                expCls <- getCls $ expectedCid
                actCls <- getCls $ expCid eexe
                throwAt pos ("Value of type `" ++ (className expCls) ++ "' expected, `" ++
                             (className actCls) ++ "' found")
            return $ mkExe $ \_ -> execRValue eexe $ \val re -> reReturn re val re
        Nothing ->
            throwAt pos ("Unexpected return statement " ++
                                    "outside of function body")

stmtSem (Stmt_Assert (Tok_Assert (pos, _)) expr) = do
    eexe <- exprSem expr
    fpos <- completePos pos
    return $ mkExe $ \k -> execRValue eexe $ \val -> do
        if isTruthy val then
            k ()
        else
            \_ _ -> do
                hPutStrLn stderr ("Assertion at " ++ (showPos fpos) ++
                                  " failed: " ++ (printTree expr))
                runtimeFailure

stmtSem (Stmt_Print {}) = do
    return $ mkExe $ \k re mem -> do
        putChar '\n'
        k () re mem

stmtSem (Stmt_PrintValues (Tok_Print (_pos, _)) exprs) = do
    exprExes <- mapM exprSem exprs
    return $ doPrint exprExes
    where
        doPrint [exprExe] = mkExe $ \k -> do
            execRValue exprExe $ \val re mem -> do
                putStr (show val)
                putChar '\n'
                k () re mem
        doPrint (exprExe:exprExes) = mkExe $ \k -> do
            execRValue exprExe $ \val re mem -> do
                putStr (show val)
                putChar ' '
                exec (doPrint exprExes) k re mem
        doPrint [] = error "doPrint []"

stmtSem Stmt_Memdump = do
    return $ mkExe $ \k re mem -> do
        hPutStrLn stderr $ show mem
        k () re mem

-- Stmt_LetTuple.      Stmt ::= "let" "(" [LowerIdent] ")" "=" Expr ;  -- tuple unpacking
-- Stmt_Case.          Stmt ::= "case" Expr "class" "of" "{" [CaseClause] "}";
-- Stmt_ForIn.         Stmt ::= "for" LowerIdent "in" Expr StatementBlock ;
-- Stmt_For.           Stmt ::= "for" LowerIdent "=" Expr "then" Expr StatementBlock ;
-- Stmt_ForWhile.      Stmt ::= "for" LowerIdent "=" Expr "then" Expr "while" Expr StatementBlock ;
stmtSem stmt = notYet $ printTree stmt



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                                                                            --
--                             typechecking                                   --
--                                                                            --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


data TypeExpr = ClsExpr {
                    typeCid :: Cid
              } | StrExpr {
                    typeSid :: Sid,
                    typeCid :: Cid
              }

typeExprSem :: Expr -> Semantic TypeExpr
typeExprSem (Expr_TypeName (UpperIdent (pos, typeName))) = do
    maybeSid <- asks $ (M.lookup typeName).envStructs
    maybeCid <- asks $ (M.lookup typeName).envClasses
    when (isNothing maybeSid && isNothing maybeCid) $ do
        throwAt pos ("Undefined class name: `" ++ typeName ++ "'")
    let cid = fromJust maybeCid
    case maybeSid of
        Just sid -> do
            return $ StrExpr sid cid
        Nothing -> do
            return $ ClsExpr cid
typeExprSem (Expr_FnType sgnExpr@(FnSignature_ _ _ _)) = do
    (fnSgn, _, _) <- fnSignatureSem sgnExpr RejectDefaults
    cid <- getFunctionCid fnSgn
    return $ ClsExpr cid
typeExprSem (Expr_TmplAppl _ (Tok_LTP (pos, _)) _) = do
    notYetAt pos "Expr_TmplAppl"
typeExprSem (Expr_NestedType _ (UpperIdent (pos, _))) = do
    notYetAt pos "Expr_NestedType"
typeExprSem (Expr_TypeVar (DollarIdent (pos, _))) = do
    notYetAt pos "Expr_TypeVar"
typeExprSem (Expr_Parens _ [expr]) = typeExprSem expr
typeExprSem expr = do
    throwError $ SErr $ "`" ++ (printTree expr) ++ "' is not a proper type name"

typeExprCid :: Expr -> Semantic Cid
typeExprCid expr = do
    typeExpr <- typeExprSem expr
    return $ typeCid typeExpr


deduceType :: Expr -> Semantic Cid
deduceType (Expr_Char _) = return (stdClss M.! "Char")
-- deduceType (Expr_String c) _ =
-- deduceType (Expr_Double d) _ = liftM RValue $ return $ ValDouble d
deduceType (Expr_Int _) = return (stdClss M.! "Int")
-- deduceType (Expr_Tuple _) _ = ???
-- deduceType (Expr_Array _) _ = ???
deduceType (Expr_Var (LowerIdent (pos, varName))) = do
    vars <- asks envVars
    case M.lookup varName vars of
        Just (VarType _ cid _) -> return cid
        Nothing -> throwAt pos $ "Undefined variable `" ++ varName ++ "'"

deduceType (Expr_Not _) = return (stdClss M.! "Bool")
deduceType (Expr_RelOper {}) = return (stdClss M.! "Bool")

deduceType (Expr_Add {}) = return (stdClss M.! "Int")
deduceType (Expr_Sub {}) = return (stdClss M.! "Int")
deduceType (Expr_Mul {}) = return (stdClss M.! "Int")
-- deduceType (Expr_Div {}) = ??? / {}
deduceType (Expr_IntDiv {}) = return (stdClss M.! "Int")
deduceType (Expr_Mod {}) = return (stdClss M.! "Int")
deduceType (Expr_Minus expr) = deduceType expr

deduceType (Expr_Lambda _ signature _) = do
    (fnSgn, _, _) <- fnSignatureSem signature AcceptDefaults
    getFunctionCid fnSgn

deduceType expr@(Expr_TypeName (UpperIdent (pos, _))) = do
    typeExpr <- typeExprSem expr
    case typeExpr of
        ClsExpr _cid -> notYetAt pos "Class reflection"
        StrExpr sid _ -> do
            struct <- gets $ (!!! sid).globStructs.sstGlob
            getFunctionCid $ structCtorSgn struct

deduceType (Expr_Field expr (LowerIdent (pos, attrName))) = do
    objCid <- deduceType expr
    structs <- gets $ globStructs.sstGlob
    case filter ((objCid ==).structCid.snd) $ M.toList structs of
        [] -> throwAt pos ("Unable to deduce struct type of expression `" ++
                           (printTree expr) ++ "'. Attrs are not accesible")
        [(_sid, struct)] -> case M.lookup attrName $ structAttrs struct of
            Just cid -> return cid
            Nothing -> throwAt pos ("Struct of type `" ++ (structName struct) ++
                                    "' has no attr named `" ++ attrName ++ "'")
        _ -> error "WTF: duplicated struct cids"

deduceType (Expr_Prop expr (LowerIdent (pos, propName))) = do
    objCid <- deduceType expr
    cls <- getCls objCid
    liftM varClass $ case M.lookup propName $ classProps cls of
        Just propType -> return propType
        Nothing -> throwAt pos (
            "Object of class `" ++ className cls ++
            "' has no property named `" ++ propName ++ "'")

deduceType (Expr_FunCall expr (Tok_LP (pos, _)) _) = do
    funCid <- deduceType expr
    classes <- gets $ globClasses.sstGlob
    case classes !!! funCid of
        ClassFun fnSgn -> do
            case mthRetType fnSgn of
                Nothing -> return $ stdClss M.! "Void"
                Just cid -> return cid
        ClassDesc {className_=clsName} -> do
            -- TODO: in future, being function derivative should be enough
            throwAt pos ("Trying to call `" ++ (printTree expr) ++ "' " ++
                         "of type `" ++ clsName ++ "', which is not a function")

deduceType (Expr_Parens _ [expr]) = deduceType expr

-- deduceType (Expr_Parens exprs) = {- tuple -}

deduceType expr = notYet $ "deduceType for " ++ (printTree expr)


getCls :: Cid -> Semantic ClassDesc
getCls cid = gets $ (!!! cid).globClasses.sstGlob


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                                 _____                                      --
--           ___  _  ______  _____/ ___/___  ____ ___                         --
--          / _ \| |/_/ __ \/ ___/\__ \/ _ \/ __ `__ \                        --
--         /  __/>  </ /_/ / /   ___/ /  __/ / / / / /                        --
--         \___/_/|_/ .___/_/   /____/\___/_/ /_/ /_/                         --
--                 /_/                                                        --
--                                                                            --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


exprSem :: Expr -> Semantic ExprSem
exprSem (Expr_Char c) = return $ RValue (stdClss M.! "Char") $ mkExe ($ ValChar c)
-- exprSem (Expr_String c) _ =
-- exprSem (Expr_Double d) _ = liftM RValue $ return $ ValDouble d
exprSem (Expr_Int i) = return $ RValue (stdClss M.! "Int") $ mkExe ($ ValInt $ fromInteger i)
-- exprSem (Expr_Tuple _) _ = ???
-- exprSem (Expr_Array _) _ = ???
exprSem (Expr_Var (LowerIdent (pos, varName))) = do
    vars <- asks envVars
    case M.lookup varName vars of
        Just (VarType mutable cid _) -> do
            let exeGet = do
                 mkExe $ \kv re mem -> kv (getVar varName mem) re mem
            if mutable then do
                let exeSet pt = do
                    mkExe $ \ku re mem -> ku () re (assignVar varName pt mem)
                return $ LValue cid exeGet exeSet
            else do
                return $ RValue cid exeGet
        Nothing -> throwAt pos $ "Undefined variable `" ++ varName ++ "'"

exprSem (Expr_Not expr) = do
    exee <- exprSem expr
    return $ RValue (stdClss M.! "Bool") $ mkExe $ \ke -> do
        execRValue exee $ \(ValBool bval) -> do
            ke $ ValBool $ not bval

exprSem expr@(Expr_RelOper {}) = do
    sem' <- exprSem' expr
    return $ RValue (stdClss M.! "Bool") $ mkExe $ \ke -> do
        exec sem' $ \maybeVal -> do
            ke $ ValBool $ isJust maybeVal
    where
        exprSem' :: Expr -> Semantic (Exe (Maybe VarVal))
        exprSem' (Expr_RelOper exp1 oper exp2) = do
            e1exe <- exprSem' exp1
            e2exe <- exprSem exp2
            return $ mkExe $ \kmv -> do
                exec e1exe $ \maybeVal -> do
                    case maybeVal of
                        Just val1 -> do
                            execRValue e2exe $ \val2 -> do
                                if operFun (asInt val1) (asInt val2) then
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
                        op -> error ("operator " ++ (printTree op) ++
                                     " not implemented yet")
        exprSem' expr' = do
            exee <- exprSem expr'
            return $ mkExe $ \kmv -> do
                execRValue exee $ \v -> kmv $ Just v

exprSem (Expr_Add exp1 exp2) = intBinopSem (+) exp1 exp2
exprSem (Expr_Sub exp1 exp2) = intBinopSem (-) exp1 exp2
exprSem (Expr_Mul exp1 exp2) = intBinopSem (*) exp1 exp2
-- exprSem (Expr_Div exp1 exp2) = ??? / exp1 exp2
exprSem (Expr_IntDiv exp1 exp2) = intBinopSem div exp1 exp2
exprSem (Expr_Mod exp1 exp2) = intBinopSem mod exp1 exp2
exprSem (Expr_Minus expr) = do
    exee <- exprSem expr
    return $ RValue (expCid exee) $ mkExe $ \ke -> do
        execRValue exee $ \(ValInt val) re mem -> do
            ke (ValInt $ 0 - val) re mem

exprSem (Expr_Lambda (Tok_Fn (_pos, _)) signature bodyBlock) = do
    (fnSgn, defArgs, argDefPoss) <- fnSignatureSem signature CompileDefaults
    vars <- asks envVars
    let makeInEnv outEnv = outEnv{
            envVars=M.union vars (argsToVars (mthArgs fnSgn) argDefPoss),
            envExpectedReturnType=Just $ mthRetType fnSgn,
            envInsideLoop=False
        }
    exeBody <- local makeInEnv $ stmtBlockSem bodyBlock
    cid <- getFunctionCid fnSgn
    let lambdaVal closureFid = do
        ValFunction $ FunImpl $ mkCall fnSgn defArgs exeBody closureFid
    return $ RValue cid $ mkExe $ \ke re mem -> do
        ke (lambdaVal $ memFid mem) re mem

exprSem expr@(Expr_TypeName (UpperIdent (pos, _))) = do
    typeExpr <- typeExprSem expr
    case typeExpr of
        ClsExpr _cid -> notYetAt pos "Class reflection"
        StrExpr sid _ -> do
            struct <- gets $ (!!! sid).globStructs.sstGlob
            cid <- getFunctionCid $ structCtorSgn struct
            return $ RValue cid $ mkExe $ \ke -> do
                ke $ ValFunction $ structCtor struct


exprSem (Expr_Field expr (LowerIdent (pos, attrName))) = do
    exee <- exprSem expr
    let objCid = expCid exee
    structs <- gets $ globStructs.sstGlob
    cid <- case filter ((objCid ==).structCid.snd) $ M.toList structs of
        [] -> throwAt pos ("Unable to deduce struct type of expression `" ++
                           (printTree expr) ++ "'. Attrs are not accesible")
        [(_sid, struct)] -> case M.lookup attrName $ structAttrs struct of
            Just cid -> return cid
            Nothing -> throwAt pos ("Struct of type `" ++ (structName struct) ++
                                    "' has no attr named `" ++ attrName ++ "'")
        _ -> error "WTF: duplicated struct cids"
    let
        exeGet = mkExe $ exeAttr $ \_objPt attrs kv -> do
            kv (attrs M.! attrName)
        exeSet val = mkExe $ exeAttr $ \objPt attrs ku re mem -> let
            mem' = memAdjust mem objPt $ \objVal -> objVal{
                objAttrs=(M.insert attrName val attrs)
            }
            in ku () re mem'
        exeAttr :: (MemPt -> M.Map VarName VarVal -> SemiCont a) -> SemiCont a
        exeAttr handler ka =  do
            execRValue exee $ \(ValRef objPt) re mem -> let
                attrs = objAttrs $ memObjAt mem objPt
                in handler objPt attrs ka re mem
    return $ LValue cid exeGet exeSet

exprSem (Expr_Prop expr (LowerIdent (pos, propName))) = do
    exee <- exprSem expr
    let objCid = expCid exee
    cls <- getCls objCid
    propType <- case M.lookup propName $ classProps cls of
        Just propType -> return propType
        Nothing -> throwAt pos (
            "Object of class `" ++ className cls ++
            "' has no property named `" ++ propName ++ "'")
    let
        propCid = varClass propType
        exeGet = exeProp $ \prop objPt -> do
            exec $ propGetter prop objPt
        exeSet val = exeProp $ \prop objPt -> do
            exec $ propSetter prop objPt val
        exeProp :: NFData a => (PropImpl -> MemPt -> SemiCont a) -> Exe a
        exeProp handler = mkExe $ \ka -> do
            execRValue exee $ \(ValRef objPt) re mem -> let
                struct = objStruct (memObjAt mem objPt) re
                impl = (structClasses struct) !!! (structCid struct) -- objCid
                prop = impl !!! propName
                in (handler prop objPt) ka re mem
    if varMutable propType then
        return $ LValue propCid exeGet exeSet
    else
        return $ RValue propCid exeGet

exprSem (Expr_FunCall expr (Tok_LP (pos, _)) args) = do
    exeFn <- exprSem expr
    argTuples <- mapM argSem args
    kwargs <- mapM unJustKwarg $ dropWhile fstIsNothing argTuples
    let kwargsSet = foldl (\s (n, _) -> S.insert n s) S.empty kwargs
    when ((S.size kwargsSet) /= (length kwargs)) $ do
        throwError $ SErr "Kwarg names not unique"
    classes <- gets $ globClasses.sstGlob
    cid <- case classes !!! (expCid exeFn) of
        ClassFun fnSgn -> do
            case mthRetType fnSgn of
                Nothing -> return $ stdClss M.! "Void"
                Just cid -> return cid
        ClassDesc {className_=clsName} -> do
            -- TODO: in future, being function derivative should be enough
            throwAt pos ("Trying to call `" ++ (printTree expr) ++ "' " ++
                         "of type `" ++ clsName ++ "', which is not a function")
    return $ RValue cid $ mkExe $ \kv -> do
        execRValue exeFn $ \(ValFunction fnImpl) -> do
            exec (funBody fnImpl argTuples) kv
    where
        argSem (FunCallArg_Positional argExpr) = do
            exeArg <- liftM expRValue $ exprSem argExpr
            return (Nothing, exeArg)
        argSem (FunCallArg_Keyword (LowerIdent (_, name)) _ argExpr) = do
            exeArg <- liftM expRValue $ exprSem argExpr
            return (Just name, exeArg)
        fstIsNothing = (== Nothing).fst
        unJustKwarg (Just name, exe) = return (name, exe)
        unJustKwarg (Nothing, _) = do
            throwError $ SErr "Positional arg after kwargs"

exprSem (Expr_Parens _ [expr]) = exprSem expr

-- exprSem (Expr_Parens exprs) = {- tuple -}

exprSem expr = notYet $ printTree expr



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                                    __  _ __                                --
--                             __  __/ /_(_) /____                            --
--                            / / / / __/ / / ___/                            --
--                           / /_/ / /_/ / (__  )                             --
--                           \__,_/\__/_/_/____/                              --
--                                                                            --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data DefaultsHandling = RejectDefaults
                      | AcceptDefaults
                      | CompileDefaults

fnSignatureSem :: FnSignature -> DefaultsHandling
    -> Semantic (FunSgn, M.Map VarName (Exe VarVal), M.Map VarName CodePosition)
fnSignatureSem (FnSignature_ _ args optResType) defaultsHandling = do
    argTuples <- mapM argSem args
    retType <- case optResType of
        OptResultType_None -> return Nothing
        OptResultType_Some typeExpr -> do
            liftM Just $ typeExprCid typeExpr
    let funSgn = FunSgn {
        mthRetType=retType,
        mthArgs=(map (\(a, _, _) -> a) argTuples)
    }
    let defArgsMap = foldl argToMap M.empty argTuples
    let argDefPosMap = foldl argToDefPosMap M.empty argTuples
    return (funSgn, defArgsMap, argDefPosMap)
    where
        argSem :: FunDef_Arg -> Semantic (ArgSgn, Maybe (Exe VarVal), CodePosition)
        argSem (FunDef_Arg_ typeExpr (LowerIdent (pos, "_")) defVal) = do
            fpos <- completePos pos
            case defVal of
                MaybeDefaultVal_Some _ _ -> do
                    throwAt pos ("Unnamed arguments cannot have " ++
                                            "default value specified (may " ++
                                            "change in future)")
                MaybeDefaultVal_None -> return ()
            cid <- typeExprCid typeExpr
            return $ (ArgSgn {
                argName=Nothing,
                argType=cid,
                argHasDefault=False
            }, Nothing, fpos)
        argSem (FunDef_Arg_ typeExpr (LowerIdent (pos, name)) defVal) = do
            fpos <- completePos pos
            -- TODO: process type
            (hasDef, defExe) <- case defVal of
                MaybeDefaultVal_None -> return (False, Nothing)
                MaybeDefaultVal_Some _ expr -> do
                    case defaultsHandling of
                        AcceptDefaults -> do
                            return (True, Nothing)
                        CompileDefaults -> do
                            defExe <- liftM expRValue $  exprSem expr
                            return (True, Just $ defExe)
                        RejectDefaults -> do
                            throwAt pos ("Default argument value " ++
                                        "specification is not allowed here")
            cid <- typeExprCid typeExpr
            return $ (ArgSgn {
                argName=Just name,
                argType=cid,
                argHasDefault=hasDef
            }, defExe, fpos)
        argToMap m (_, Nothing, _) = m
        argToMap m (ArgSgn{argName=Just name}, Just exe, _) = M.insert name exe m
        argToMap _ _ = error "argToMap: default value for unnamed argument"
        argToDefPosMap m (ArgSgn{argName=Nothing}, _, _dp) = m
        argToDefPosMap m (ArgSgn{argName=Just name}, _, dp) = M.insert name dp m


argsToVars :: [ArgSgn] -> M.Map VarName CodePosition -> M.Map VarName VarType
argsToVars args argDefPoss = M.fromList $ do
    arg <- args
    case arg of
        ArgSgn (Just name) cid _ -> do
            return $ (name, VarType {
                varMutable=True,
                varClass=cid,
                varDefPos=argDefPoss !!! name
            })
        ArgSgn Nothing _ _ -> []

mkCall :: FunSgn -> M.Map VarName (Exe VarVal) -> Exe () -> Fid ->
    [(Maybe VarName, Exe VarVal)] -> Exe VarVal
mkCall fnSgn defArgs exeBody closureFid argTuples = do
    mkExe $ \kpt re0 mem0 -> let
        doReturn pt re mem = kpt pt re{reReturn=reReturn re0} (setFid (memFid mem0) mem)
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
    return $ RValue (stdClss M.! "Int") $ mkExe $ \ke -> do
        execRValue e1exe $ \(ValInt v1) -> do
            execRValue e2exe $ \(ValInt v2) -> do
                ke $ ValInt $ op v1 v2

assertTmplSgnEmpty :: MaybeTemplateSgn -> Semantic ()
assertTmplSgnEmpty mTmplSgn =
    case mTmplSgn of
        MaybeTemplateSgn_Some _ (_:_) -> notYet "Templates"
        _ -> return ()

throwAt :: (Int, Int) -> String -> Semantic a
throwAt (ln, col) msg = do
    fileName <- gets sstFileName
    throwError $ SErrP (fileName, ln, col) msg

notYet :: Show a => a -> Semantic b
notYet = throwError.(SErr).("not yet: " ++).show

notYetAt :: Show a => (Int, Int) -> a -> Semantic b
notYetAt pos what = throwAt pos $ "not yet: " ++ (show what)
