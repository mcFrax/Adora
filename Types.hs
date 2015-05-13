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
} deriving Show

type Cid = Int -- class id
type Sid = Int -- struct id
type CTid = Int
type STid = Int

type CidMap = M.Map Cid ClassDesc
type SidMap = M.Map Sid StructDesc

data LocEnv = LocEnv {
    envSuper :: Maybe LocEnv,
    envVars :: M.Map VarName VarType,
    envClasses :: M.Map VarName Cid,
    envStructs :: M.Map VarName Sid,
    envExpectedReturnType :: Maybe (Maybe Cid),
    envInsideLoop :: Bool -- whether break and continue are legal
} deriving Show

data GlobEnv = GlobEnv {
    globClasses :: CidMap,
    globStructs :: SidMap
} deriving Show

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
} deriving Show

data FunSgn = FunSgn {
    mthRetType :: Maybe Cid,
    mthArgs :: [ArgSgn]
} deriving (Eq, Ord, Show)

data ArgSgn = ArgSgn {
    argName :: Maybe VarName,
    argType :: Cid,
    argHasDefault :: Bool
} deriving (Eq, Ord, Show)

data StructDesc = StructDesc {
    structCid :: Cid,
    structAttrs :: M.Map VarName Cid,
    structClasses :: M.Map Cid Impl
} deriving Show

type Impl = M.Map VarName PropImpl  -- both props and mths

data PropImpl = PropImpl {
    propGetter :: Pointer -> Either (Exe Value) (Exe Pointer),
    propSetter :: Pointer -> Pointer -> Exe ()  -- possibly undefined/error
}

instance Show PropImpl where
    show _ = "<PropImpl>"

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
type SemiCont a = (a -> Cont) -> Cont

newtype Exe a = Exe {
    exec :: SemiCont a
}

mkExe :: SemiCont a -> Exe a
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

io :: IO a -> SemiCont a
io ioOp ka re mem = do
    iores <- ioOp
    ka iores re mem

noop :: Exe ()
noop = mkExe ($ ())
