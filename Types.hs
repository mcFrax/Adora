module Types where

import Control.DeepSeq
import Control.Exception
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Exit(exitWith, ExitCode(..))
import System.IO

-- -- --
-- Environment
-- -- --

type CodePosition = (String, Int, Int)

internalCodePos :: CodePosition
internalCodePos = ("<internal>", 0, 0)

type VarName = String
data VarType = VarType {
    varMutable :: Bool,
    varClass :: Cid,
    varStatic :: Bool, -- is it visible in methods?
    -- varInitialized :: Bool, --TODO
    varDefPos :: CodePosition
} deriving Show

newtype Cid = Cid Int deriving (Eq, Ord, Show) -- class id
newtype Sid = Sid Int deriving (Eq, Ord, Show) -- struct id
newtype CTid = CTid Int deriving (Eq, Ord, Show)
newtype STid = STid Int deriving (Eq, Ord, Show)

type CidMap = M.Map Cid ClassDesc
type SidMap = M.Map Sid StructDesc

data LocEnv = LocEnv {
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
    className_ :: String,
    classOwnProps :: M.Map VarName VarType,
    classProps_ :: M.Map VarName VarType,
    classDirectSupers :: S.Set Cid,
    classAllSupers :: S.Set Cid
} | ClassFun FunSgn
    deriving Show

className :: ClassDesc -> VarName
className (ClassDesc{className_=name}) = name
className (ClassFun sgn) = do
    "<(" ++ (intercalate ", " args) ++ ") -> " ++ (show $ mthRetType sgn) ++ ">"
    where
        args = flip map (mthArgs sgn) $ \as -> do
            "<" ++ (show $ argType as) ++ "> " ++ case argName as of
                Just name -> name
                Nothing -> "_"

classProps :: ClassDesc -> M.Map VarName VarType
classProps (ClassDesc{classProps_=props}) = props
classProps (ClassFun _) = M.empty


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
    structDirectSupers :: [Sid],
    structMRO :: [Sid],
    structOwnAttrs :: M.Map VarName Cid,  -- attrs defined in this struct
    structAttrs :: M.Map VarName Cid,
    structDirImpl :: M.Map VarName PropImpl,  -- prop impls defined/overriden in this struct
    structImpl :: Impl,
    structCtor :: FunImpl,
    structCtorSgn :: FunSgn
} deriving Show

type Impl = M.Map VarName (PropImpl, Maybe PropImpl)  -- both props and mths

data PropImpl = PropImpl {
    propGetter :: MemPt -> Exe VarVal,
    propSetter :: MemPt -> VarVal -> Exe (),  -- possibly undefined/error
    propDefPos :: CodePosition
}

instance Show PropImpl where
    show _ = "<PropImpl>"

data FunImpl = FunImpl {
    funSgn :: FunSgn,
    funBody :: [(Maybe VarName, Exe VarVal)] -> Exe VarVal
}

instance Show FunImpl where
    show fImpl = "<function " ++ (show $ funSgn fImpl) ++ ">"


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
    frameParentId :: Fid,
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
            runtimeFailure

runExe :: Exe a -> Cont
runExe exe = exec exe (\_ _ _ -> return ())

noop :: Exe ()
noop = mkExe ($ ())


data ExprSem = RValue {
                expCid :: Cid,
                expRValue :: Exe VarVal
            } | LValue {
                expCid :: Cid,
                expRValue :: Exe VarVal,
                setLValue :: VarVal -> Exe ()
            }

execRValue :: ExprSem -> SemiCont VarVal
execRValue = exec.expRValue

runtimeFailure :: IO a
runtimeFailure = exitWith $ ExitFailure 2
