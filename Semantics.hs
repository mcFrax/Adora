{-# OPTIONS -XMultiParamTypeClasses #-}

module Semantics where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Debug.Trace

import System.IO

import Absadora
import Printadora(printTree)

import Memory
import Types
import StdLib


trace :: Show a => String -> a -> b -> b
trace pref trac val = Debug.Trace.trace (pref ++ (show trac)) val

#define TRACE(x) (trace (__FILE__ ++ ":" ++ (show (__LINE__ :: Int))) (x) $ seq (x) $ return ())


data SemState = SemState {
    sstGlob :: GlobEnv,
    sstFunClasses :: M.Map FunSgn Cid,
    sstNextCid :: Cid,
    sstNextSid :: Sid,
    sstFileName :: String
}

newCid :: TCM Cid
newCid = do
    sst <- get
    let cid@(Cid cidVal) = sstNextCid sst
    put sst{sstNextCid=Cid $ cidVal + 1}
    return cid

newSid :: TCM Sid
newSid = do
    sst <- get
    let sid@(Sid sidVal) = sstNextSid sst
    put sst{sstNextSid=Sid $ sidVal + 1}
    return sid

getFunctionCid :: FunSgn -> TCM Cid
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


newtype TCM a = TCM {
    runTCM :: SemState -> LocEnv -> Either Err (SemState, LocEnv, a)
}

data Err = Err CodePosition ErrType String
         | ErrSomewhere String

data ErrType = ErrUndefinedType VarName
             | ErrOther

instance Monad TCM where
    tcm1 >>= tcm2 = TCM $ \st env -> do
        (st', env', x) <- runTCM tcm1 st env
        runTCM (tcm2 x) st' env'
    return x = TCM $ \st env -> return (st, env, x)

instance MonadState SemState TCM where
    get = TCM $ \st env -> return (st, env, st)
    put st = TCM $ \_ env -> return (st, env, ())

instance MonadReader LocEnv TCM where
    ask = TCM $ \st env -> return (st, env, env)
    local f (TCM run) = TCM $ \st env -> do
        (st', _, res) <- run st (f env)
        return (st', env, res)

modifyGlob :: (GlobEnv -> GlobEnv) -> TCM ()
modifyGlob f = modify $ \sst -> sst{sstGlob=f $ sstGlob sst}

-- environment modifications (tampering with Reader part):

setEnv :: LocEnv -> TCM ()
setEnv env = TCM $ \st _ -> return (st, env, ())

modifyEnv :: (LocEnv -> LocEnv) -> TCM ()
modifyEnv f = setEnv =<< (liftM f ask)

-- errors:

throwError :: Err -> TCM a
throwError e = TCM $ \_ _ -> Left e

throwAt :: (Int, Int) -> String -> TCM a
throwAt (ln, col) msg = do
    fileName <- gets sstFileName
    throwError $ Err (fileName, ln, col) ErrOther msg

typeMismatch :: Cid -> Cid -> (Int, Int) -> TCM a
typeMismatch expectedCid foundCid pos = do
    expName <- liftM className $ getCls expectedCid
    fndName <- liftM className $ getCls foundCid
    throwAt pos ("Value of type `" ++ expName ++ "' expected, `" ++
                 fndName ++ "' found")

notYet :: Show a => a -> TCM b
notYet = throwError.(ErrSomewhere).("not yet: " ++).show

notYetAt :: Show a => (Int, Int) -> a -> TCM b
notYetAt pos what = throwAt pos $ "not yet: " ++ (show what)

trySem :: TCM a -> TCM (Either Err a)
trySem tcm = TCM $ \st env -> do
    case runTCM tcm st env of
        Right (st', env', x) -> Right (st', env', Right x)
        Left err -> Right (st, env, Left err)

showSemError :: Err -> String
showSemError (Err pos _ s) = do
    (showPos pos) ++ ": error: " ++ s
showSemError (ErrSomewhere s) = "?:?:?: error: " ++ s

showPos :: CodePosition -> String
showPos (path, ln, col) = path ++ ":" ++ (show ln) ++ ":" ++ (show col)

completePos :: (Int, Int) -> TCM CodePosition
completePos (ln, col) = do
    fileName <- gets sstFileName
    return (fileName, ln, col)

-- global env:

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
            classProps_=M.empty,
            classDirectSupers=S.empty,
            classAllSupers=S.empty
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

moduleSem :: Module -> String -> Either Err (IO ())
moduleSem (Module_ stmts) fileName = do
    let Module_ stdlibStmts = stdlib
    (sst0, env0, exe0) <- do
        case runTCM (stmtSeqSem stdlibStmts) initSemState initEnv of
            Right res -> return res
            Left errmsg -> do
                error ("Error in standard library:\n" ++
                       (showSemError errmsg))
    (sst, _, exe1) <- runTCM (stmtSeqSem stmts) sst0{sstFileName=fileName} env0
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
            sstFunClasses=M.empty,
            sstNextCid=Cid 0,
            sstNextSid=Sid 0,
            sstFileName="<stdlib>"
        }

        initEnv = LocEnv {
            envVars=M.fromList [
                ("true", VarType True (stdClss M.! "Bool") internalCodePos),
                ("false", VarType False (stdClss M.! "Bool") internalCodePos)
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
                frameParentId=error "Trying to get top frame's parent",
                frameContent=M.empty
            }

stmtBlockSem :: StatementBlock -> TCM (Exe ())
stmtBlockSem (StatementBlock_ stmts) = local id $ stmtSeqSem stmts

stmtSeqSem :: [Stmt] -> TCM (Exe ())
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


type AliasSpec = (CodePosition, VarName, Expr)

type HoistingExtras = ([AliasSpec], S.Set Sid)

data HoistingLevel = HoistingMore HoistingExtras (TCM HoistingLevel)
                   | HoistingEnd HoistingExtras

hoisted :: [Stmt] -> TCM (Exe a) -> TCM (Exe a)
hoisted stmts innerSem = do
    level1 <- (foldl (>>=) (return $ HoistingEnd ([], S.empty))) =<< (mapM hoistStmt stmts)
    hoist level1
    innerSem
    where
        hoist (HoistingMore (aliases, sids) nextLevel) = do
            makeAliases aliases
            resolveMros sids
            nextLevel' <- nextLevel
            hoist nextLevel'
        hoist (HoistingEnd (aliases, sids)) = do
            makeAliases aliases
            resolveMros sids
        makeAliases [] = return ()
        makeAliases aliases = do
            let aliases' = flip map aliases $ \(fpos, name, expr) -> do
                    (fpos, name, expr, Nothing)
                leftNames = S.fromList $ map (\(_, name, _) -> name) aliases
            makeAliases' aliases' leftNames

        makeAliases' :: [(CodePosition, VarName, Expr, Maybe VarName)]
            -> S.Set VarName -> TCM ()
        makeAliases' aliases leftNames = do
            (aliases', leftNames') <- do
                foldr (=<<) (return ([], leftNames)) $ map tryMakeAlias aliases
            if S.null leftNames then do
                return ()
            else if (S.size leftNames') < (S.size leftNames) then do
                makeAliases' aliases' leftNames'
            else do
                let aliasLines = flip map aliases $ \(fpos, name, expr, _) -> do
                    ("\n" ++ (showPos fpos) ++ "      type " ++
                     name ++ " = " ++ (printTree expr))
                let (fpos, _, _, _) = head aliases
                throwError $ Err fpos ErrOther (
                    "Unable to resolve type aliases:" ++ (foldr (++) "" aliasLines))

        tryMakeAlias alias@(fpos, name, expr, knownDep) (aliases, leftNames) = do
            let noKnownDeps = do
                case knownDep of
                    Nothing -> True
                    Just dep -> not $ S.member dep leftNames
            if noKnownDeps then do
                mType <- tryDeduce expr
                let success = return (aliases, S.delete name leftNames)
                case mType of
                    Right (StrExpr sid cid) -> do
                        strAlias sid
                        clsAlias cid
                        success
                    Right (ClsExpr cid) -> do
                        clsAlias cid
                        success
                    Left (depName, err) -> do
                        if S.member depName leftNames then do
                            let alias' = (fpos, name, expr, Just depName)
                            return (alias':aliases, leftNames)
                        else do
                            throwError err  -- this type is not going to be
                                            -- found, rethrow
            else do
                return (alias:aliases, leftNames)
            where
                strAlias sid = do
                    structs <- asks envStructs
                    case tryInsert name sid structs of
                        Right structs' -> do
                            modifyEnv $ \env -> env{envStructs=structs'}
                        Left _collidingSid ->
                            throwError $ Err fpos ErrOther (
                                "Struct `" ++ name ++ "' already defined")
                clsAlias cid = do
                    classes <- asks envClasses
                    case tryInsert name cid classes of
                        Right classes' -> do
                            modifyEnv $ \env -> env{envClasses=classes'}
                        Left _collidingCid ->
                            throwError $ Err fpos ErrOther (
                                "Class `" ++ name ++ "' already defined")
        tryDeduce expr = do
            mTypeExpr <- trySem $ typeExprSem expr
            case mTypeExpr of
                Right typeExpr -> return $ Right typeExpr
                Left err@(Err _ (ErrUndefinedType name) _) -> return $ Left (name, err)
                Left err -> throwError err

        resolveMros sids = do
            _ <- resolveMros' sids S.empty sids
            return ()

        resolveMros' sids stack unresolved = do
            if S.null sids then do
                return unresolved
            else do
                let (sid, sids') = S.deleteFindMin sids
                unresolved' <- do
                    if S.member sid unresolved then do
                        resolveMro sid stack unresolved
                    else do
                        return unresolved
                resolveMros' (S.intersection sids' unresolved') stack unresolved'

        resolveMro sid stack unresolved = do
            when (S.member sid stack) $ do
                throwError $ ErrSomewhere (
                    "Unable to resolve inheritance: dependency cycle")
            struct <- gets $ (!!! sid).globStructs.sstGlob
            let directs = structDirectSupers struct
            unresolved' <- resolveMros' (S.fromList directs) (S.insert sid stack) unresolved
            directsMros <- forM directs $ \direct -> do
                gets $ structMRO.(!!! direct).globStructs.sstGlob
            mro <- merge [sid] directsMros
            modifyGlob $ \glob -> do
                glob{
                    globStructs=M.insert sid struct{
                            structMRO=mro
                        } $ globStructs glob
                }
            return $ S.delete sid unresolved'
        merge acc [] = return $ reverse acc
        merge acc mros = do
            let heads = map head mros
                tails = concat $ map tail mros
                candidates = filter (`notElem` tails) heads
            case candidates of
                sid:_ -> do
                    let mros' = flip mapMaybe mros $ \mro -> do
                        case mro of
                            [] -> Nothing
                            m@[sid'] -> do
                                if sid' == sid then Nothing else Just m
                            m@(sid':sids) -> do
                                if sid' == sid then Just sids else Just m
                    merge (sid:acc) mros'
                [] -> do
                    throwError $ ErrSomewhere (
                        "Unable to resolve inheritance: MRO confict")

-- queueNextLevel, queueNextLevels, queueAlias i hoistingDone są mechanizmem
-- kolejkowania kolejnych etapów hoistingu. Dzieki takiemu rozwiązaniu, kod
-- hoistingu każdego rodzaju deklaracji jest spójną sekwencją, mimo, że jego
-- wykonanie jest kilkukrotnie przeplatane z wykonaniem akcji dla innych
-- deklaracji.

-- Jako, że rozwiązanie aliasów musi nastąpić na konkretnym etapie, a wymaga
-- obróbki wszystkich zdefiniowanych aliasów jednocześnie, queueAlias umożliwia
-- zbieranie zbioru aliasów w celu wspólnej obsługi. Rozwiązanie można
-- łatwo rozszerzyć na zbieranie innego typu zadań na dowolnym etapie.

-- EDIT: Doszedł queueStruct, robiący coś podobnego dla rozwiązywania
-- dziedziczenia, i możliwe, że będzie jeszcze coś podobnego dla klas.

queueNextLevel :: TCM (HoistingLevel -> TCM HoistingLevel)
    -> TCM (HoistingLevel -> TCM HoistingLevel)
queueNextLevel nextLevel = do
    return $ \hoistingAcc -> do
        nextLevel' <- nextLevel
        case hoistingAcc of
            HoistingMore extras hoistingAcc' -> do
                return $ HoistingMore extras $ do
                    hoistingAcc' >>= nextLevel'
            HoistingEnd extras -> do
                return $ HoistingMore extras $ nextLevel' $ HoistingEnd ([], S.empty)

queueNextLevels :: Integer -> TCM (HoistingLevel -> TCM HoistingLevel)
    -> TCM (HoistingLevel -> TCM HoistingLevel)
queueNextLevels n queued
    | n <= 0 = queued
    | otherwise = queueNextLevels (n-1) $ queueNextLevel queued

queueAlias :: AliasSpec -> TCM (HoistingLevel -> TCM HoistingLevel)
queueAlias alias = do
    return $ \hoistingAcc -> do
        case hoistingAcc of
            HoistingMore (aliases, sids) hoistingAcc' -> do
                return $ HoistingMore (alias:aliases, sids) hoistingAcc'
            HoistingEnd (aliases, sids) -> do
                return $ HoistingEnd (alias:aliases, sids)

queueStruct :: Sid -> TCM (HoistingLevel -> TCM HoistingLevel)
queueStruct sid = do
    return $ \hoistingAcc -> do
        case hoistingAcc of
            HoistingMore (aliases, sids) hoistingAcc' -> do
                return $ HoistingMore (aliases, S.insert sid sids) hoistingAcc'
            HoistingEnd (aliases, sids) -> do
                return $ HoistingEnd (aliases, S.insert sid sids)

hoistingDone :: TCM (HoistingLevel -> TCM HoistingLevel)
hoistingDone = return $ return

hoistStmt :: Stmt -> TCM (HoistingLevel -> TCM HoistingLevel)
hoistStmt (Stmt_Let ident _ expr) = do
    queueNextLevels 5 $ do
        cid <- deduceType expr
        hoistVar cid False ident
hoistStmt (Stmt_LetTuple {}) = do
    notYet $ "hoistStmt (Stmt_LetTuple ..)"
hoistStmt (Stmt_Var ident _ expr) = do
    queueNextLevels 5 $ do
        cid <- deduceType expr
        hoistVar cid True ident
hoistStmt (Stmt_AliasDef (TypeDefinition_Alias ident _ typeExpr)) = do
    let UpperIdent (pos, aliasName) = ident
    fpos <- completePos pos
    queueAlias (fpos, aliasName, typeExpr)

hoistStmt (Stmt_ClassDef clsDef@(TypeDefinition_Class {})) = do
    let (TypeDefinition_Class ident mTmplSgn maybeSupers variants decls) = clsDef
    let UpperIdent (pos, clsName) = ident
    let superClsExprs = extractMaybeSupers maybeSupers
    beforeClasses <- asks envClasses
    when (clsName `M.member` beforeClasses) $
        throwAt pos ("Class `" ++ clsName ++ "' already defined")
    assertTmplSgnEmpty mTmplSgn
    cid <- newCid
    modifyEnv $ \env -> env {
            envClasses=M.insert clsName cid beforeClasses
        }
    let updateDesc newDesc = do
        modifyGlob $ \glob -> do
            glob{
                globClasses=M.insert cid newDesc $ globClasses glob
            }
    queueNextLevels 2 $ do
        superClses <- forM superClsExprs $ \(SuperType_ t) -> do
            sem <- typeExprSem t
            case sem of
                ClsExpr superCid -> return superCid
                StrExpr {} -> throwAt pos $ (
                    "`" ++ (printTree t) ++ "' is struct, abstract class expected")
        let directSupers = S.fromList superClses
        cls <- compileClass cid ident mTmplSgn directSupers variants decls
        updateDesc cls
        queueNextLevel $ do
            allSupers <- closeSuperClss directSupers
            updateDesc cls{
                classAllSupers=allSupers
            }
            hoistingDone

hoistStmt (Stmt_StructDef strDef@(TypeDefinition_Struct {})) = do
    let (TypeDefinition_Struct ident mTmplSgn maybeSupers decls) = strDef
    let UpperIdent (strPos, strName) = ident
    let superStrExprs = extractMaybeSupers maybeSupers
    beforeClasses <- asks envClasses
    beforeStructs <- asks envStructs
    when (strName `M.member` beforeClasses) $
        throwAt strPos ("Class `" ++ strName ++ "' already defined")
    when (strName `M.member` beforeStructs) $
        throwAt strPos ("Struct `" ++ strName ++ "' already defined")
    assertTmplSgnEmpty mTmplSgn
    sid <- newSid
    cid <- newCid
    modifyEnv $ \env -> env {
            envStructs=M.insert strName sid beforeStructs,
            envClasses=M.insert strName cid beforeClasses
        }
    let updateDesc newDesc = do
            modifyGlob $ \glob -> do
                glob{
                    globStructs=M.insert sid newDesc $ globStructs glob
                }
        updateCls newDesc = do
            modifyGlob $ \glob -> do
                glob{
                    globClasses=M.insert cid newDesc $ globClasses glob
                }
    queueNextLevels 2 $ do  -- 2
        superStrs <- forM superStrExprs $ \(SuperType_ t) -> do
            typeSem <- typeExprSem t
            case typeSem of
                StrExpr {} -> return typeSem
                ClsExpr {} -> throwAt strPos $ (
                    "`" ++ (printTree t) ++ "' is abstract class, struct expected")
        let directSuperStrs = map typeSid superStrs
        let superStrClss = map typeCid superStrs
        implClss <- liftM catMaybes $ forM decls $ \decl -> do
            case decl of
                InStruct_ImplDefinition impClsExpr _implDecls -> do
                    liftM Just $ typeExprCid impClsExpr
                _ -> return Nothing
        let directSuperClsss = S.fromList $ superStrClss ++ implClss
        -- TODO verify inheritance correctness - eliminate implementic struct classes without deriving from them
        clsDecls <- liftM catMaybes $ mapM (hoistStrClsDecl cid) decls
        cls <- compileClass cid ident mTmplSgn directSuperClsss [] clsDecls
        updateCls cls
        queueNextLevel $ do  -- 3
            allSuperClss <- closeSuperClss directSuperClsss
            updateCls cls{
                classAllSupers=allSuperClss
            }
            revAttrs <- foldl (>>=) (return []) $ map (hoistStrAttr cid) decls
            let attrs = reverse revAttrs
                attrsMap = M.fromList attrs
                structStub = StructDesc {
                    structName=strName,
                    structCid=cid,
                    structDirectSupers=directSuperStrs,
                    structMRO=error "Undefined structMRO",
                    structOwnAttrs=attrsMap,
                    structAttrs=error "Undefined structAttrs",
                    structClasses=error "Undefined structClasses",
                    structCtor=error "Undefined structCtor",
                    structCtorSgn=error "Undefined structCtorSgn"
                }
            updateDesc structStub
            q1 <- queueStruct sid
            q2 <- queueNextLevels 2 $ do  -- 5
                structStub' <- gets $ (!!! sid).globStructs.sstGlob
                let mergeAttrs acc [] = return acc
                    mergeAttrs acc (sid':sids) = do
                        attrs' <- gets $ structOwnAttrs.(!!! sid').globStructs.sstGlob
                        let acc' = M.union acc attrs'
                        if (M.size acc') == ((M.size acc) + (M.size attrs')) then do
                            mergeAttrs acc' sids
                        else do
                            throwAt strPos "Conflicting attrs"
                mergedAttrsMap <- mergeAttrs M.empty $ structMRO structStub'
                let struct = structStub' {
                        structCtor=FunImpl ctorSgn constructor,
                        structAttrs=mergedAttrsMap,
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
                updateDesc struct
                queueNextLevel $ do -- 6
                    let initImpls = S.fold insertEmpty M.empty $ S.insert cid allSuperClss
                        insertEmpty iCid m = M.insert iCid M.empty m
                    directImpls <- foldl (>>=) (return initImpls) $ map (hoistStrDecl cid) decls
                    impls <- completeImpls directImpls cid $ map typeSid superStrs
                    updateDesc struct {
                        structClasses=impls
                    }
                    hoistingDone
            return $ q1 >=> q2
hoistStmt _stmt = hoistingDone


extractMaybeSupers :: MaybeSupers -> [SuperType]
extractMaybeSupers MaybeSupers_None = []
extractMaybeSupers (MaybeSupers_Some _ supers) = supers

completeImpls :: M.Map Cid Impl -> Cid -> [Sid] -> TCM (M.Map Cid Impl)
completeImpls impls _ownCid _superSids = return impls


hoistVar :: Cid -> Bool -> LowerIdent -> TCM (HoistingLevel -> TCM HoistingLevel)
hoistVar cid mutable (LowerIdent (pos, varName)) = do
    do
        vars <- asks envVars
        fpos <- completePos pos
        case M.lookup varName vars of
            Just var -> do
                throwAt pos (
                    "variable redefined: `" ++ varName ++ "'\n" ++
                    (showPos $ varDefPos var) ++ ": Previously defined here")
            Nothing -> do
                let var = VarType {
                    varClass=cid,
                    varMutable=mutable,
                    varDefPos=fpos
                }
                modifyEnv $ \env -> env{envVars=M.insert varName var vars}
                hoistingDone


hoistStrAttr :: Cid -> InStruct -> [(VarName, Cid)]
    -> TCM [(VarName, Cid)]
hoistStrAttr _ decl@(InStruct_AttrDefinition {}) attrs = do
    let (InStruct_AttrDefinition
            typeExpr
            (LowerIdent (pos, name))
            _maybeDefault) = decl
    cid <- typeExprCid typeExpr
    when (elem name $ map fst attrs) $ do
        throwAt pos $ "Attribute `" ++ name ++ "' already defined"
    return $ (name, cid):attrs
hoistStrAttr _ _ attrs = return attrs

hoistStrDecl :: Cid -> InStruct -> (M.Map Cid Impl) -> TCM (M.Map Cid Impl)
hoistStrDecl _ (InStruct_AttrDefinition {}) impls = return impls
hoistStrDecl ownCid (InStruct_InImplDecl inImpl) impls = do
    hoistImplDecl ownCid inImpl impls
hoistStrDecl _ (InStruct_ImplDefinition clsName inImpls) impls = do
    implCid <- typeExprCid clsName
    foldl (>>=) (return impls) $ map (hoistImplDecl implCid) inImpls
hoistStrDecl _ (InStruct_AliasDef {}) _ = do
    notYet "Nested types"
hoistStrDecl _ (InStruct_ClassDef {}) _ = do
    notYet "Nested types"
hoistStrDecl _ (InStruct_StructDef {}) _ = do
    notYet "Nested types"

hoistImplDecl :: Cid -> InImpl -> (M.Map Cid Impl) -> TCM (M.Map Cid Impl)
hoistImplDecl _ InImpl_Pass impls = return impls
hoistImplDecl implCid mthDef@(InImpl_MethodDefinition {}) impls = do
    let (InImpl_MethodDefinition
            (LowerIdent (pos, name))
            mTmplSgn
            header
            bodyBlock) = mthDef
    fpos <- completePos pos
    assertTmplSgnEmpty mTmplSgn
    (fnSgn, defArgs, argDefPoss) <- fHeaderSem header CompileDefaults
--                     outerVars <- asks envVars
    let argVars = M.insert "self" (VarType {
            varMutable=False,
            varClass=implCid,
            varDefPos=fpos
        }) $ argsToVars (mthArgs fnSgn) argDefPoss
    let makeInEnv outEnv = outEnv{
            envVars=argVars, -- M.union outerVars argVars,
            envExpectedReturnType=Just $ mthRetType fnSgn,
            envInsideLoop=False
        }
    exeBody <- local makeInEnv $ stmtBlockSem bodyBlock
    let propImpl = mkMth fpos $ \pt -> FunImpl fnSgn $ \args -> do
            let args' = (Just "self", mkExe $ \kv-> kv $ ValRef pt):args
            mkCall fnSgn defArgs exeBody topLevelFid args'
    addPropToImpls name pos propImpl [implCid] impls
hoistImplDecl implCid propDecl@(InImpl_PropertyDefinition {}) impls = do
    let (InImpl_PropertyDefinition
            typeExpr
            (LowerIdent (pos, name))
            getDef
            maybeSet) = propDecl
    cid <- typeExprCid typeExpr
    fpos <- completePos pos
    getter <- case getDef of
        (PropDefClause_Def bodyBlock) -> do
            let getterSgn = FunSgn {
                mthRetType=Just cid,
                mthArgs=[]
            }
--                          outerVars <- asks envVars
            let makeInEnv outEnv = outEnv{
                    envVars=M.fromList [("self", VarType {
                        varMutable=False,
                        varClass=implCid,
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
            notYetAt pos "auto getter"
    setter <- case maybeSet of
        MaybeSetClause_None -> return $ error "Read-only property"
        MaybeSetClause_Some (PropDefClause_Def _bodyBlock) -> do
            notYetAt pos "custom setter"
        MaybeSetClause_Some PropDefClause_Auto -> do
            notYetAt pos "auto setter"
    let propImpl = PropImpl {
        propGetter=getter,
        propSetter=setter,
        propDefPos=fpos
    }
    addPropToImpls name pos propImpl [implCid] impls


mkMth :: CodePosition -> (MemPt -> FunImpl) -> PropImpl
mkMth fpos mth = PropImpl {
    propGetter=(\pt -> mkExe ($ ValFunction $ mth pt)),
    propSetter=(error "Methods are read-only properties"),
    propDefPos=fpos
}


compileClass :: Cid -> UpperIdent -> MaybeTemplateSgn -> S.Set Cid -> [VariantDefinition]
    -> [InClass] -> TCM ClassDesc
compileClass _cid ident mTmplSgn directSupers variants decls = do
    let UpperIdent (clsPos, clsName) = ident
    assertTmplSgnEmpty mTmplSgn
    when (not $ null variants) $ notYetAt clsPos "variants"
    props <- foldl (>>=) (return M.empty) $ map hoistClsDecl decls
    let cls = ClassDesc {
            className_=clsName,
            classProps_=props,
            classDirectSupers=directSupers,
            classAllSupers=error "Undefined classAllSupers"
        }
    return cls

hoistStrClsDecl :: Cid -> InStruct -> TCM (Maybe InClass)
hoistStrClsDecl _ownCid (InStruct_InImplDecl mthDef@(InImpl_MethodDefinition {})) = do
    let (InImpl_MethodDefinition ident mTmplSgn fHeader _bodyBlock) = mthDef
    return $ Just (InClass_MethodDeclaration ident mTmplSgn $ fHdrToFSgn fHeader)
    where
        fHdrToFSgn (FHeader_ lp args optResType) = do
            let args' = flip map args $ \(ArgDefinition_ typeExpr argIdent mDefVal) -> do
                let optionality = case mDefVal of
                        MaybeDefaultVal_None -> ArgMandatory
                        MaybeDefaultVal_Some {} -> ArgOptional
                ArgSignature_ typeExpr argIdent optionality
            FSignature_ lp args' optResType
hoistStrClsDecl _ownCid (InStruct_InImplDecl propDecl@(InImpl_PropertyDefinition {})) = do
    let (InImpl_PropertyDefinition
            typeExpr
            (LowerIdent (pos, name))
            _getDef
            maybeSet) = propDecl
    let propMutability = do
        if maybeSet == MaybeSetClause_None
            then PropReadOnly
            else PropWritable
    return $ Just (InClass_PropertyDeclaration
            propMutability
            typeExpr
            (LowerIdent (pos, name)))
hoistStrClsDecl _ _ = return Nothing

hoistClsDecl :: InClass -> M.Map VarName VarType
    -> TCM (M.Map VarName VarType)
hoistClsDecl propDecl@(InClass_PropertyDeclaration {}) props = do
    let (InClass_PropertyDeclaration
            propMutability
            typeExpr
            (LowerIdent (pos, name))) = propDecl
    propCid <- typeExprCid typeExpr
    fpos <- completePos pos
    let propType = VarType {
            varMutable=propMutability == PropWritable,
            varClass=propCid,
            varDefPos=fpos
        }
    addPropToMap name pos propType props
hoistClsDecl (InClass_AliasDef {}) _ = do
    notYet "Nested types"
hoistClsDecl (InClass_ClassDef {}) _ = do
    notYet "Nested types"
hoistClsDecl decl@(InClass_MethodDeclaration {}) props = do
    let (InClass_MethodDeclaration
            (LowerIdent (pos, name))
            mTmplSgn
            signature) = decl
    fpos <- completePos pos
    assertTmplSgnEmpty mTmplSgn
    (fnSgn, _) <- fSignatureSem signature
    propCid <- getFunctionCid fnSgn
    let propType = VarType {
            varMutable=False,
            varClass=propCid,
            varDefPos=fpos
        }
    addPropToMap name pos propType props
hoistClsDecl InClass_Pass props = return props

addPropToImpls :: VarName -> (Int, Int) -> PropImpl -> [Cid] -> M.Map Cid Impl
    -> TCM (M.Map Cid Impl)
addPropToImpls name pos propImpl cids impls0 = do
    foldl (>>=) (return impls0) (map addProp' cids)
    where
        addProp' cid impls = do
            impl' <- addPropToMap name pos propImpl (impls !!! cid)
            return $ M.insert cid impl' impls

addPropToMap :: VarName -> (Int, Int) -> a -> M.Map VarName a
    -> TCM (M.Map VarName a)
addPropToMap name pos propImpl propMap = do
    case tryInsert name propImpl propMap of
        Right propMap' -> return propMap'
        Left _ -> do
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


stmtSem :: Stmt -> TCM (Exe ())
stmtSem (Stmt_ClassDef _) = return noop
stmtSem (Stmt_StructDef _) = return noop
stmtSem (Stmt_AliasDef _) = return noop
stmtSem Stmt_Pass = return noop

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
                typeMismatch (expCid lexe) (expCid rexe) pos
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
                typeMismatch expectedCid (expCid eexe) pos
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
--     __                          __         __           __  _              --
--    / /___  ______  ___     ____/ /__  ____/ /_  _______/ /_(_)___  ____    --
--   / __/ / / / __ \/ _ \   / __  / _ \/ __  / / / / ___/ __/ / __ \/ __ \   --
--  / /_/ /_/ / /_/ /  __/  / /_/ /  __/ /_/ / /_/ / /__/ /_/ / /_/ / / / /   --
--  \__/\__, / .___/\___/   \__,_/\___/\__,_/\__,_/\___/\__/_/\____/_/ /_/    --
--     /____/_/                                                               --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


data TypeExpr = ClsExpr {
                    typeCid :: Cid
              } | StrExpr {
                    typeSid :: Sid,
                    typeCid :: Cid
              }

typeExprSem :: Expr -> TCM TypeExpr
typeExprSem (Expr_TypeName (UpperIdent (pos, typeName))) = do
    maybeSid <- asks $ (M.lookup typeName).envStructs
    maybeCid <- asks $ (M.lookup typeName).envClasses
    when (isNothing maybeSid && isNothing maybeCid) $ do
        fpos <- completePos pos
        throwError $ Err fpos (ErrUndefinedType typeName) $ (
            "Undefined class name: `" ++ typeName ++ "'")
    let cid = fromJust maybeCid
    case maybeSid of
        Just sid -> do
            return $ StrExpr sid cid
        Nothing -> do
            return $ ClsExpr cid
typeExprSem (Expr_FnType sgnExpr@(FSignature_ _ _ _)) = do
    (fnSgn, _) <- fSignatureSem sgnExpr
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
    throwError $ ErrSomewhere $ "`" ++ (printTree expr) ++ "' is not a proper type name"

typeExprCid :: Expr -> TCM Cid
typeExprCid expr = do
    typeExpr <- typeExprSem expr
    return $ typeCid typeExpr


deduceType :: Expr -> TCM Cid
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
    (fnSgn, _, _) <- fHeaderSem signature AcceptDefaults
    getFunctionCid fnSgn

deduceType expr@(Expr_TypeName (UpperIdent (pos, _))) = do
    typeExpr <- typeExprSem expr
    case typeExpr of
        ClsExpr _cid -> notYetAt pos "Class reflection"
        StrExpr sid _ -> do
            struct <- gets $ (!!! sid).globStructs.sstGlob
            getFunctionCid $ structCtorSgn struct

deduceType (Expr_Attr expr (LowerIdent (pos, attrName))) = do
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


getCls :: Cid -> TCM ClassDesc
getCls cid = gets $ (!!! cid).globClasses.sstGlob


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--                                 _____                                      --
--           ___  _  ______  _____/ ___/___  ____ ___                         --
--          / _ \| |/_/ __ \/ ___/\__ \/ _ \/ __ `__ \                        --
--         /  __/>  </ /_/ / /   ___/ /  __/ / / / / /                        --
--         \___/_/|_/ .___/_/   /____/\___/_/ /_/ /_/                         --
--                 /_/                                                        --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


exprSem :: Expr -> TCM ExprSem
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

exprSem (Expr_As expr (Tok_As (pos, _)) asExpr) = do
    exee <- exprSem expr
    let subCid = expCid exee
    superCid <- typeExprCid asExpr
    ok <- isSubclass subCid superCid
    when (not ok) $ do
        classes <- gets $ globClasses.sstGlob
        let subCls = classes !!! subCid
            superCls = classes !!! superCid
        throwAt pos ("Illegal cast: expression`" ++ (printTree expr) ++
                     "' has type `" ++ (className subCls) ++ "', which " ++
                     "is not a subclass of `" ++ (className superCls) ++ "'")
    return $ RValue superCid $ expRValue exee

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
        exprSem' :: Expr -> TCM (Exe (Maybe VarVal))
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
    (fnSgn, defArgs, argDefPoss) <- fHeaderSem signature CompileDefaults
    vars <- asks envVars
    let makeInEnv outEnv = outEnv{
            envVars=M.union vars (argsToVars (mthArgs fnSgn) argDefPoss),
            envExpectedReturnType=Just $ mthRetType fnSgn,
            envInsideLoop=False
        }
    exeBody <- local makeInEnv $ stmtBlockSem bodyBlock
    cid <- getFunctionCid fnSgn
    let lambdaVal closureFid = do
        ValFunction $ FunImpl fnSgn $ mkCall fnSgn defArgs exeBody closureFid
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


exprSem (Expr_Attr expr (LowerIdent (pos, attrName))) = do
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
        throwAt pos "Kwarg names not unique"
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
            throwAt pos "Positional arg after kwargs"

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

data DefaultsHandling = AcceptDefaults
                      | CompileDefaults

fHeaderSem :: FHeader -> DefaultsHandling
    -> TCM (FunSgn, M.Map VarName (Exe VarVal), M.Map VarName CodePosition)
fHeaderSem (FHeader_ _ args optResType) defaultsHandling = do
    argTuples <- mapM argSem args
    retType <- case optResType of
        OptResultType_None -> return Nothing
        OptResultType_Some typeExpr -> do
            liftM Just $ typeExprCid typeExpr
    let fnSgn = FunSgn {
        mthRetType=retType,
        mthArgs=(map (\(a, _, _) -> a) argTuples)
    }
    let defArgsMap = foldl argToMap M.empty argTuples
    let argDefPosMap = M.fromList $ flip mapMaybe argTuples $ \(as, _, dp) -> do
        (argName as) >>= (\n -> Just (n, dp))
    return (fnSgn, defArgsMap, argDefPosMap)
    where
        argSem :: ArgDefinition -> TCM (ArgSgn, Maybe (Exe VarVal), CodePosition)
        argSem (ArgDefinition_ typeExpr (LowerIdent (pos, name)) defVal) = do
            let unnamed = (name == "_")
            argCid <- typeExprCid typeExpr
            fpos <- completePos pos
            (hasDef, defExe) <- case defVal of
                MaybeDefaultVal_None -> return (False, Nothing)
                MaybeDefaultVal_Some _ expr -> do
                    when unnamed $ do
                        notYetAt pos ("Unnamed argument with default value")
                    case defaultsHandling of
                        AcceptDefaults -> do
                            defCid <- deduceType expr
                            when (defCid /= argCid) $ do
                                typeMismatch argCid defCid pos
                            return (True, Nothing)
                        CompileDefaults -> do
                            defSem <- exprSem expr
                            when ((expCid defSem) /= argCid) $ do
                                typeMismatch argCid (expCid defSem) pos
                            return (True, Just $ expRValue defSem)
            return $ (ArgSgn {
                argName=if unnamed then Nothing else Just name,
                argType=argCid,
                argHasDefault=hasDef
            }, defExe, fpos)
        argToMap m (_, Nothing, _) = m
        argToMap m (ArgSgn{argName=Just name}, Just exe, _) = M.insert name exe m
        argToMap _ _ = error "argToMap: default value for unnamed argument"

fSignatureSem :: FSignature -> TCM (FunSgn, M.Map VarName CodePosition)
fSignatureSem (FSignature_ _ args optResType) = do
    argTuples <- mapM argSem args
    retType <- case optResType of
        OptResultType_None -> return Nothing
        OptResultType_Some typeExpr -> do
            liftM Just $ typeExprCid typeExpr
    let fnSgn = FunSgn {
        mthRetType=retType,
        mthArgs=(map fst argTuples)
    }
    let argDefPosMap = M.fromList $ flip mapMaybe argTuples $ \(as, dp) -> do
        (argName as) >>= (\n -> Just (n, dp))
    return (fnSgn, argDefPosMap)
    where
        argSem :: ArgSignature -> TCM (ArgSgn, CodePosition)
        argSem (ArgSignature_ typeExpr (LowerIdent (pos, name)) isOpt) = do
            argCid <- typeExprCid typeExpr
            fpos <- completePos pos
            return $ (ArgSgn {
                argName=if name == "_" then Nothing else Just name,
                argType=argCid,
                argHasDefault=isOpt == ArgOptional
            }, fpos)


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


intBinopSem :: (Int -> Int -> Int) -> Expr -> Expr -> TCM ExprSem
intBinopSem op exp1 exp2 = do
    e1exe <- exprSem exp1
    e2exe <- exprSem exp2
    return $ RValue (stdClss M.! "Int") $ mkExe $ \ke -> do
        execRValue e1exe $ \(ValInt v1) -> do
            execRValue e2exe $ \(ValInt v2) -> do
                ke $ ValInt $ op v1 v2

assertTmplSgnEmpty :: MaybeTemplateSgn -> TCM ()
assertTmplSgnEmpty mTmplSgn =
    case mTmplSgn of
        MaybeTemplateSgn_Some _ (_:_) -> notYet "Templates"
        _ -> return ()

tryInsert :: Ord k => k -> a -> M.Map k a -> Either a (M.Map k a)
tryInsert key val m = do
    case M.insertLookupWithKey (\_ _ x -> x) key val m of
        (Nothing, m') -> Right m'
        (Just colliding, _) -> Left colliding

isSubclass :: Cid -> Cid -> TCM Bool
isSubclass subCid superCid = do
    if subCid == superCid then do
        return True
    else do
        classes <- gets $ globClasses.sstGlob
        let subCls = classes !!! subCid
            superCls = classes !!! superCid
        case subCls of
            ClassDesc {} -> case superCls of
                ClassDesc {} -> do
                    return $ S.member superCid $ classAllSupers subCls
                ClassFun _ -> notYet "Function inheritance"
            ClassFun _ -> notYet "Function inheritance"

closeSuperClss :: S.Set Cid -> TCM (S.Set Cid)
closeSuperClss directSupers = do
    classes <- gets $ globClasses.sstGlob
    let closeSuperClss' acc nextLevelSet = do
            if S.null nextLevelSet then
                return acc
            else do
                -- Quite inefficient, but who cares? And that's simple.
                let newCids = nextLevelSet S.\\ acc
                let acc' = S.union acc newCids
                let nextLevelSet' = S.unions $ do
                    cid <- S.toList newCids
                    return $ classDirectSupers (classes !!! cid)
                closeSuperClss' acc' nextLevelSet'

    closeSuperClss' S.empty directSupers
