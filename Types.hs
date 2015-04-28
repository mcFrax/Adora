module Types where

import qualified Data.Map as M

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
    classMths :: M.Map String FunDesc
}

data FunDesc = FunDesc {
    mthRetType :: Cid,
    mthArgs :: [(Maybe VarName, Cid, Maybe Pointer)]
}

data StructDesc = StructDesc {
    structAttrs :: M.Map VarName Cid,
    structClasses :: M.Map Cid Impl
}

data Impl = Impl {
    implProps :: M.Map VarName PropImpl,
    implMths :: M.Map VarName FunImpl
}

data PropImpl = PropImpl {
    propGetter :: Maybe FunImpl,
    propSetter :: Maybe FunImpl
}

data FunImpl = FunImpl {
    mthDesc :: FunDesc,
    mthBody :: Exe Value
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

-- -- --
-- Execution
-- -- --

type Cont = RunEnv -> Memory -> IO ()

type Exe a = (a -> Cont) -> Cont

runExe :: Exe a -> Cont
runExe exe = exe (\_ _ _ -> return ())

io :: IO a -> Exe a
io ioOp ka re mem = do
    iores <- ioOp
    ka iores re mem

noop :: Exe ()
noop = ($ ())
