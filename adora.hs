module Main where

import Control.Monad.Error
import qualified Data.Map as M
import System.Environment
import System.Exit(exitFailure)
import System.IO

import Absadora
import ErrM
import Paradora

type VarName = String
type ClassName = String
type StructName = String

type Fid = Int
type Pointer = Int
data Memory = Memory {
    memFid :: Fid,  -- current frame fid
    memNext :: Pointer,
    memNextFid :: Fid,
    memValues :: M.Map Pointer Value,
    memFrames :: M.Map Fid Frame
} deriving Show

type FrameKey = VarName
data Frame = Frame {
    frameParentId :: Fid,
    frameContent :: M.Map FrameKey Pointer  -- frame index -> memory index
} deriving Show

data Value = ValVoid
           | ValInt { valToInt :: Int }
           | ValChar { valToChar :: Char }
           | ValRef { valToRef :: Pointer }
           | ValObject {
--                 valObjStruct :: StructDesc,
                valObjAttrs :: AttrDict
             } deriving Show
type AttrDict = M.Map VarName Pointer

memFrame :: Memory -> Frame
memFrame mem = (memFrames mem) M.! (memFid mem)

memGet :: Memory -> Pointer -> Value
memGet mem pt = (memValues mem) M.! pt

memSet :: Memory -> Pointer -> Value -> Memory
memSet mem pt v = mem{memValues=M.insert pt v (memValues mem)}

setFid :: Fid -> Memory -> Memory
setFid fid mem = mem{memFid=fid}

alloc :: Value -> Memory -> (Pointer, Memory)
alloc v mem = do
    let pt = memNext mem
    (pt, mem{
        memNext=(pt + 1),
        memValues=M.insert pt v (memValues mem)
    })

allocFrame :: Memory -> (Fid, Memory)
allocFrame mem = do
    let frame = Frame{
        frameParentId=memFid mem,
        frameContent=M.empty
    }
    let fid = memNextFid mem
    (fid, mem{
        memNextFid=(fid+1),
        memFrames=M.insert fid frame $ memFrames mem
    })

-- frameGet :: Frame -> FrameKey -> Memory -> Value
-- frameGet f k mem = memGet mem $ frameGetPt f k
--
-- frameGetPt :: Frame -> FrameKey -> Pointer
-- frameGetPt f k = (frameContent f) M.! k

getVarPt :: FrameKey -> Memory -> Pointer
getVarPt k mem = do
    getVarPt' $ memFrame mem
    where
        frames = memFrames mem
        getVarPt' f = do
            case M.lookup k (frameContent f) of
                Just pt -> pt
                Nothing -> getVarPt' (frames M.! (frameParentId f))

getVar :: FrameKey -> Memory -> Value
getVar k mem = memGet mem $ getVarPt k mem

allocVar :: FrameKey -> Value -> Memory -> Memory
allocVar k v mem = do
    allocVarFid (memFid mem) k v mem

allocVarFid :: Fid -> FrameKey -> Value -> Memory -> Memory
allocVarFid fid k v mem = do
    let (pt, mem') = alloc v mem
    let frames = memFrames mem
    let frame = frames M.! fid
    let frame' = frame{frameContent=M.insert k pt $ frameContent frame}
    mem'{memFrames=M.insert fid frame' frames}

assignVar :: FrameKey -> Value -> Memory -> Memory
assignVar k v mem = do
    memSet mem (getVarPt k mem) v


data Env = Env {
    envSuper :: Maybe Env,
    envVars :: M.Map VarName VarType,
    envClasses :: M.Map ClassName ClassDesc,
    envStructs :: M.Map StructName StructDesc,
    envExpectedReturnType :: Maybe VarType
}

type Cont = Memory -> IO ()
type ContE = Value -> Cont
type ContD = Env -> Cont


type VarType = ClassDesc
-- data VarType = RefVar ClassDesc
--              | ValVar StructName

data ClassTemplate = ClassTemplate {
    clsTemplArgNames :: [String],
    clsTemplInstantiate :: [ClassDesc] -> ClassDesc
}

data ClassDesc = ClassDesc {
    classProps :: M.Map VarName PropDesc,
    classMths :: M.Map String MthDesc
}

data PropDesc = PropDesc {
    propGetType :: Maybe VarType,
    propSetType :: Maybe VarType
}

data MthDesc = MthDesc {
    mthRetType :: VarType,
    mthArgs :: [(VarName, VarType, Maybe VarName)]
}

data StructDesc = StructDesc {
    structAttrs :: M.Map VarName VarType,
    structMths :: M.Map VarName MthImpl,
    structClasses :: M.Map ClassName Impl
}

data Impl = Impl {
    implProps :: M.Map VarName PropImpl,
    implMths :: M.Map VarName MthImpl
}

data PropImpl = PropImpl {
    propGetter :: Maybe MthImpl,
    propSetter :: Maybe MthImpl
}

data MthImpl = MthImpl {
    mthDesc :: MthDesc,
    mthBody :: ContE -> Cont
}

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
    memNext=0,
    memNextFid=1,
    memValues=M.empty,
    memFrames=M.fromList [(0, globFrame)]
} where
    globFrame = Frame{
        frameParentId=0-1,
        frameContent=M.empty
    }

moduleSem :: Module -> SemanticErrorOr (IO ())
moduleSem (Module_ stmts) = do
    msem <- stmtBlockSem (StatementBlock_ stmts) globalEnv
    return $ msem (\_ -> return ()) emptyMem

notYet :: String -> a
notYet s = error $ "Tego jeszcze nie ma: " ++ s

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

stmtSem _ _ = notYet "stmtSem"

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

exprSem _ _ = notYet "exprSem"

intBinopSem :: (Int -> Int -> Int) -> Expr -> Expr -> Env ->
    SemanticErrorOr (ContE -> Cont)
intBinopSem op exp1 exp2 env = do
    e1sem <- exprSem exp1 env
    e2sem <- exprSem exp2 env
    return $ \ke ->
        e1sem $ \(ValInt v1) ->
            e2sem $ \(ValInt v2) ->
                ke $ ValInt $ v1 `op` v2

declSem :: Decl -> Env -> SemanticErrorOr (Env, (Cont -> Cont))
declSem = notYet "declSem"

type SemanticErrorOr s = Either SemanticError s

data SemanticError = SemanticError String

instance Error SemanticError where
  strMsg = SemanticError

showSemError :: SemanticError -> String
showSemError (SemanticError s) = "SemanticError: " ++ s


main :: IO ()
main = do
    args <- getArgs
    code <- case args of
        [] -> do
            getContents
        [programPath] -> do
            readFile programPath
        _ -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage:\n    " ++ progName ++ " [FILE PATH]\n"
            exitFailure
    case pModule $ myLexer $ code of
        Bad errmsg -> do
            hPutStrLn stderr $ "Parser error:\n" ++ errmsg ++ "\n"
            exitFailure
        Ok moduleSyntax -> do
            case moduleSem moduleSyntax of
                Left errmsg -> do
                    hPutStrLn stderr $ showSemError errmsg
                    exitFailure
                Right runModule -> runModule
