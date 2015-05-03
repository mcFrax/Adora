module Types where

import Control.Exception
import qualified Data.Map.Strict as M

-- -- --
-- Environment
-- -- --

type VarName = String
data VarType = VarType {
    varMutable :: Bool,
    varClass :: Cid
}

type Cid = Int -- class id
type Sid = Int -- struct id

type CidMap = M.Map Cid ClassDesc
type SidMap = M.Map Sid StructDesc

data LocEnv = LocEnv {
    envSuper :: Maybe LocEnv,
    envVars :: M.Map VarName VarType,
    envClasses :: M.Map VarName Cid,
    envStructs :: M.Map VarName Sid,
    envExpectedReturnType :: Maybe Cid
}

data GlobEnv = GlobEnv {
    globClasses :: CidMap,
    globStructs :: SidMap
}

data RunEnv = RunEnv {
    reStructs :: SidMap,
    reReturn :: Value -> Cont,
    reBreak :: Cont,
    reContinue :: Cont
}

data ClassDesc = ClassDesc {
    className :: String,
    classProps :: M.Map VarName VarType,
    classMths :: M.Map VarName FunSgn
}

data FunSgn = FunSgn {
    mthRetType :: Cid,
    mthArgs :: [ArgSgn]
} deriving (Eq, Ord)

data ArgSgn = ArgSgn {
    argName :: Maybe VarName,
    argType :: Cid,
    argHasDefault :: Bool
} deriving (Eq, Ord)

data StructDesc = StructDesc {
    structAttrs :: M.Map VarName Cid,
    structClasses :: M.Map Cid Impl
}

data Impl = Impl {
    implProps :: M.Map VarName PropImpl,
    implMths :: M.Map VarName MthImpl
}

data PropImpl = PropImpl {
    propGetter :: Pointer -> Either (Exe Value) (Exe Pointer),
    propSetter :: Pointer -> Pointer -> Exe ()  -- possibly undefined/error
}

data MthImpl = MthImpl {
    mthDesc :: FunSgn,
    mthBody :: Pointer -> [(Maybe VarName, Exe Pointer)] -> Either (Exe Value) (Exe Pointer)
}

bindMth :: MthImpl -> Pointer -> FunImpl
bindMth (MthImpl d b) pt = FunImpl d (b pt)

data FunImpl = FunImpl {
    funDesc :: FunSgn,
    funBody :: [(Maybe VarName, Exe Pointer)] -> Either (Exe Value) (Exe Pointer)
}

instance Show FunImpl where
    show _ = "<FunImpl>"


-- -- --
-- Memory
-- -- --

type Fid = Int
type Pointer = Int
data Memory = Memory {
    memFid :: Fid,  -- current frame fid
    memValues :: M.Map Pointer Value,
    memFrames :: M.Map Fid Frame
} deriving Show

type FrameKey = VarName
data Frame = Frame {
    frameParentId :: Maybe Fid,
    frameContent :: M.Map FrameKey Pointer  -- frame index -> memory index
} deriving Show

data Value = ValNull
           | ValBool { valToBool :: Bool }
           | ValFunction FunImpl
           | ValInt { valToInt :: Int }
           | ValChar { valToChar :: Char }
           | ValObject {
                valObjStruct :: Sid,
                valObjAttrs :: M.Map VarName Pointer
             } deriving Show

objStruct :: Value -> RunEnv -> StructDesc
objStruct objVal re = (reStructs re) M.! (valObjStruct objVal)


-- -- --
-- Execution
-- -- --

type Cont = RunEnv -> Memory -> IO ()

newtype Exe a = Exe {
    exec :: (a -> Cont) -> Cont
}

mkExe :: ((a -> Cont) -> Cont) -> Exe a
mkExe exe = do
    Exe $ \ka -> exe $ \a re mem -> do
        _ <- evaluate a
        _ <- evaluate re
        _ <- evaluate $ memFid mem
        _ <- evaluate $ memValues mem
        _ <- evaluate $ memFrames mem
--         putStrLn $ show mem
        ka a re mem

runExe :: Exe a -> Cont
runExe exe = exec exe (\_ _ _ -> return ())

io :: IO a -> ((a -> Cont) -> Cont)
io ioOp ka re mem = do
    iores <- ioOp
    ka iores re mem

noop :: Exe ()
noop = mkExe ($ ())
