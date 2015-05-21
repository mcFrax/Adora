module Types where

import Control.DeepSeq
import Control.Exception
import qualified Data.Map.Strict as M
import System.Exit(exitFailure)
import System.IO

import Absadora

-- -- --
-- Environment
-- -- --

type VarName = String
data VarType = VarType {
    varMutable :: Bool,
    -- varInitialized :: Bool, --TODO
    varClass :: Cid,
    varDefPos :: Maybe (Int, Int)
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

instance NFData RunEnv
-- TODO?

data ClassDesc = ClassDesc {
    className :: String,
    classProps :: M.Map VarName VarType,
    classMths :: M.Map VarName FunSgn
} | ClassDescIncomplete {
    classDecl :: (Decl, LocEnv)
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
    structName :: VarName,
    structCid :: Cid,
    structAttrs :: M.Map VarName Cid,
    structClasses :: M.Map Cid Impl,
    structCtor :: FunImpl
} deriving Show

type Impl = M.Map VarName PropImpl  -- both props and mths

data PropImpl = PropImpl {
    propGetter :: Pointer -> Either (Exe Value) (Exe Pointer),
    propSetter :: Pointer -> Pointer -> Exe ()  -- possibly undefined/error
}

instance Show PropImpl where
    show _ = "<PropImpl>"

newtype FunImpl = FunImpl {
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

instance NFData Memory
-- TODO?

type FrameKey = VarName
data Frame = Frame {
    frameParentId :: Maybe Fid,
    frameContent :: M.Map FrameKey Pointer  -- frame index -> memory index
} deriving Show

data Value = ValNull
           | ValFunction FunImpl
           | ValBool { valToBool :: Bool }
           | ValInt { valToInt :: Int }
           | ValChar { valToChar :: Char }
           | ValObject {
                valObjStruct :: Sid,
                valObjAttrs :: M.Map VarName Pointer
             }

instance Show Value where
    show ValNull = "null"
    show (ValFunction _) = "<function object>"
    show (ValBool b) = if b then "true" else "false"
    show (ValInt i) = show i
    show (ValChar c) = show c
    show (ValObject _sid _attrs) = "<object>"

instance NFData Value where
    rnf ValNull = ()
    rnf (ValFunction _) = ()
    rnf (ValBool b) = (ValBool $! b) `seq` ()
    rnf (ValInt i) = (ValInt $! i) `seq` ()
    rnf (ValChar c) = (ValChar $! c) `seq` ()
    rnf (ValObject sid attrs) = ((ValObject $! sid) $!! attrs) `seq` ()

objStruct :: Value -> RunEnv -> StructDesc
objStruct objVal re = (reStructs re) M.! (valObjStruct objVal)


class Eval e where
    eval :: e -> IO e


-- -- --
-- Execution
-- -- --

type Cont = RunEnv -> Memory -> IO ()
type SemiCont a = (a -> Cont) -> Cont

newtype Exe a = Exe {
    exec :: SemiCont a
}

mkExe :: (NFData a) => SemiCont a -> Exe a
mkExe exe = do
    Exe $!! \ka -> exe $!! \a re mem -> do
        (a', re', mem') <- handle handler $ do
            a' <- evaluate $!! a
            re' <- evaluate $!! re
            mem' <- evaluate $!! mem
            return (a', re', mem')
        ka a' re' mem'
    where
        handler :: SomeException -> IO a
        handler exception = do
            hPutStrLn stderr $ "Runtime exception: " ++ (show exception)
            exitFailure

runExe :: Exe a -> Cont
runExe exe = exec exe (\_ _ _ -> return ())

io :: IO a -> SemiCont a
io ioOp ka re mem = do
    iores <- ioOp
    ka iores re mem

noop :: Exe ()
noop = mkExe ($ ())


data ExprSem = RValue {
                expCid :: Cid,
                expRValue :: Either (Exe Value) (Exe Pointer)
            } | LValue {
                expCid :: Cid,
                expRValue :: Either (Exe Value) (Exe Pointer),
                setLValue :: Pointer -> Exe ()
            } | TypeValue {
                expCid :: Cid,
                expRValue :: Either (Exe Value) (Exe Pointer), -- reflection only
                expCls :: Maybe (Either Cid CTid),
                expStr :: Maybe (Either Sid STid)
            }
