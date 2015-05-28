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

type CodePosition = (String, Int, Int)

internalCodePos :: CodePosition
internalCodePos = ("<internal>", 0, 0)

type VarName = String
data VarType = VarType {
    varMutable :: Bool,
    -- varInitialized :: Bool, --TODO
    varClass :: Cid,
    varDefPos :: CodePosition
} deriving Show

newtype Cid = Cid Int deriving (Eq, Ord, Show) -- class id
newtype Sid = Sid Int deriving (Eq, Ord, Show) -- struct id
newtype CTid = CTid Int deriving (Eq, Ord, Show)
newtype STid = STid Int deriving (Eq, Ord, Show)

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
    reReturn :: VarVal -> Cont,
    reBreak :: Cont,
    reContinue :: Cont
}

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
    argHasDefault :: Bool,
    argDefPos :: CodePosition
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
    propGetter :: MemPt -> Exe VarVal,
    propSetter :: MemPt -> VarVal -> Exe ()  -- possibly undefined/error
}

instance Show PropImpl where
    show _ = "<PropImpl>"

newtype FunImpl = FunImpl {
    funBody :: [(Maybe VarName, Exe VarVal)] -> Exe VarVal
}

instance Show FunImpl where
    show _ = "<FunImpl>"


-- -- --
-- Memory
-- -- --

newtype Fid = Fid Int deriving (Eq, Ord, Show)
newtype MemPt = MemPt Int deriving (Eq, Ord, Show)
data Memory = Memory {
    memFid :: Fid,  -- current frame fid
    memObjects :: M.Map MemPt Object,
    memFrames :: M.Map Fid Frame
} deriving Show

type FrameKey = VarName
data Frame = Frame {
    frameParentId :: Maybe Fid,
    frameContent :: M.Map FrameKey VarVal
} deriving Show

data VarVal = ValNull
           | ValRef MemPt
           | ValFunction FunImpl
           | ValBool { asBool :: Bool }
           | ValInt { asInt :: Int }
           | ValChar { asChar :: Char }

instance Show VarVal where
    show ValNull = "null"
    show (ValRef (MemPt ptVal)) = "<object@" ++ (show ptVal) ++ ">"
    show (ValFunction _) = "<function object>"
    show (ValBool b) = if b then "true" else "false"
    show (ValInt i) = show i
    show (ValChar c) = show c

instance NFData VarVal where
    rnf ValNull = ()
    rnf (ValRef (MemPt ptVal)) = rnf ptVal
    rnf (ValFunction _) = ()
    rnf (ValBool b) = rnf b
    rnf (ValInt i) = rnf i
    rnf (ValChar c) = rnf c


isTruthy :: VarVal -> Bool
isTruthy ValNull = False
isTruthy (ValRef _) = True
isTruthy (ValFunction _) = True
isTruthy (ValBool val) = val
isTruthy (ValInt 0) = False
isTruthy (ValInt _) = True
isTruthy (ValChar '\0') = False
isTruthy (ValChar _) = True


data Object = Object {
    objSid :: Sid,
    objAttrs :: M.Map VarName VarVal
}

instance Show Object where
    show _ = "<object>"

instance NFData Object where
    rnf (Object _sid attrs) = rnf attrs

objStruct :: Object -> RunEnv -> StructDesc
objStruct obj re = (reStructs re) M.! (objSid obj)


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
        a' <- handle handler $ evaluate $!! a
        ka a' re mem
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
                expRValue :: Exe VarVal
            } | LValue {
                expCid :: Cid,
                expRValue :: Exe VarVal,
                setLValue :: VarVal -> Exe ()
            } | TypeValue {
                expCid :: Cid,
                expRValue :: Exe VarVal, -- reflection only
                expCls :: Maybe (Either Cid CTid),
                expStr :: Maybe (Either Sid STid)
            }

execRValue :: ExprSem -> SemiCont VarVal
execRValue = exec.expRValue
