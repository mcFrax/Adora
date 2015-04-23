module Adora where

import Control.Monad.Error
import qualified Data.Map as M
import System.Exit(exitFailure)

import Absadora

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

memFrame :: Memory -> Fid -> Frame
memFrame mem fid = (memFrames mem) M.! fid

memGet :: Memory -> Pointer -> Value
memGet mem pt = (memValues mem) M.! pt

memSet :: Memory -> Pointer -> Value -> Memory
memSet mem pt v = mem{memValues=M.insert pt v (memValues mem)}

alloc :: Value -> Memory -> (Pointer, Memory)
alloc v mem = do
    let pt = memNext mem
    (pt, mem{
        memNext=(pt + 1),
        memValues=M.insert pt v (memValues mem)
    })

allocFrame :: Fid -> Memory -> (Fid, Memory)
allocFrame parentFid mem = do
    let frame = Frame{
        frameParentId=parentFid,
        frameContent=M.empty
    }
    let fid = memNextFid mem
    (fid, mem{
        memNextFid=(fid+1),
        memFrames=M.insert fid frame $ memFrames mem
    })

frameParent :: Frame -> Memory -> Frame
frameParent f mem = memFrame mem $ frameParentId f

frameGet :: Frame -> FrameKey -> Memory -> Value
frameGet f k mem = memGet mem $ frameGetPt f k

frameGetPt :: Frame -> FrameKey -> Pointer
frameGetPt f k = (frameContent f) M.! k

getVarPt :: FrameKey -> Fid -> Memory -> Pointer
getVarPt k fid mem = do
    let f = memFrame mem fid
    case M.lookup k (frameContent f) of
         Just pt -> pt
         Nothing -> getVarPt k (frameParentId f) mem

getVar :: FrameKey -> Fid -> Memory -> Value
getVar k fid mem = memGet mem $ getVarPt k fid mem

allocVar :: FrameKey -> Value -> Fid -> Memory -> Memory
allocVar k v fid mem = do
    let (pt, mem') = alloc v mem
    let frames = memFrames mem
    let frame = frames M.! fid
    let frame' = frame{frameContent=M.insert k pt $ frameContent frame}
    mem'{memFrames=M.insert fid frame' frames}

assignVar :: FrameKey -> Value -> Fid -> Memory -> Memory
assignVar k v fid mem = do
    memSet mem (getVarPt k fid mem) v


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
    k <- stmtBlockSem (StatementBlock_ stmts) globalEnv
    return $ k (\_ -> return ()) emptyMem

notYet :: String -> a
notYet s = error $ "Tego jeszcze nie ma: " ++ s

stmtBlockSem :: StatementBlock -> Env -> SemanticErrorOr (Cont -> Cont)
stmtBlockSem = notYet "stmtBlockSem"
stmtSem :: Stmt -> Env -> SemanticErrorOr (Cont -> Cont)
stmtSem = notYet "stmtSem"
exprSem :: Expr -> Env -> SemanticErrorOr (ContE -> Cont)
exprSem = notYet "exprSem"
declSem :: Decl -> Env -> SemanticErrorOr (ContE -> Cont)
declSem = notYet "declSem"

type SemanticErrorOr s = Either SemanticError s

data SemanticError = SemanticError String

instance Error SemanticError where
  strMsg = SemanticError

showSemError :: SemanticError -> String
showSemError (SemanticError s) = "SemanticError: " ++ s

main :: IO ()
main = return ()
--     case stmtBlockSem (StatementBlock stmts) globalEnv of
--         Left err -> do
--             hPutStrLn stderr $ showSemError err
--             exitFailure
--         Right k ->
