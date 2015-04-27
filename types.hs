module Types where

import Control.Monad.State

import qualified Data.Map as M

-- -- --
-- Environment
-- -- --

type VarName = String

type Cid = Int -- class id
type Sid = Int -- struct id

data LocEnv = LocEnv {
    envSuper :: Maybe LocEnv,
    envVars :: M.Map VarName Cid,
    envClasses :: M.Map VarName Cid,
    envStructs :: M.Map VarName Sid,
    envExpectedReturnType :: Maybe Cid
}

data GlobEnv = GlobEnv {
    globClasses :: M.Map Cid ClassDesc,
    globStructs :: M.Map Sid StructDesc,
}

data ClassDesc = ClassDesc {
    className :: String,
    classProps :: M.Map VarName PropDesc,
    classMths :: M.Map String FunDesc
}

data PropDesc = PropDesc {
    propGetType :: Maybe Cid,
    propSetType :: Maybe Cid
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
    implMths :: M.Map VarName MthImpl
}

data PropImpl = PropImpl {
    propGetter :: Maybe MthImpl,
    propSetter :: Maybe MthImpl
}

data MthImpl = MthImpl {
    mthDesc :: FunDesc,
    mthBody :: Exe Value
}


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
    frameParentId :: Fid,
    frameContent :: M.Map FrameKey Pointer  -- frame index -> memory index
} deriving Show

data Value = ValNull
           | ValFunction MthImpl
           | ValInt { valToInt :: Int }
           | ValChar { valToChar :: Char }
           | ValRef { valToRef :: Pointer }
           | ValObject {
                valObjStruct :: Sid,
                valObjAttrs :: M.Map VarName Pointer
             } deriving Show


-- -- --
-- Execution
-- -- --

type Exe = StateT (M.Map Sid StructDesc, Memory) IO
