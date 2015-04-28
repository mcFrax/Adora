{-# OPTIONS -XMultiParamTypeClasses #-}
{-# OPTIONS -XTypeSynonymInstances #-}
{-# OPTIONS -XFlexibleInstances #-}

module Types where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class

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

newtype Exe a = Exe {
    runExe :: SidMap -> Memory -> IO (SidMap, Memory, a)
}

instance Functor Exe where
    fmap = liftM

instance Monad Exe where
    exe1 >>= exe2 = Exe $ \sm mem -> do
        (sm', mem', x) <- runExe exe1 sm mem
        runExe (exe2 x) sm' mem'
    return x = Exe $ \sm mem -> return (sm, mem, x)

instance MonadState Memory Exe where
    get = Exe $ \sm mem -> return (sm, mem, mem)
    put mem = Exe $ \sm _ -> return (sm, mem, ())

instance MonadReader SidMap Exe where
    ask = Exe $ \sm mem -> return (sm, mem, sm)
    local = undefined

instance MonadIO Exe where
    liftIO io = Exe $ \sm mem -> liftM (\x -> (sm, mem, x)) io
