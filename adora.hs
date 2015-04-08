module Adora where

import Absadora

type VarName = String
type ClassName = String
type StructName = String
type Location = Loc {
    locDepth :: Int,  -- closure depth, 0 == current
    locVal :: Int
}

type Closure = Closure {
    cloSuper :: Maybe Closure,
    cloValues :: M.Map Int Value
}

data Env = Env {
    envSuper :: Maybe Env,
    envVars :: M.Map VarName (VarType, Location),
    envClasses :: M.Map ClassName ClassDesc,
    envStructs :: M.Map StructName StructDesc,
    envExpectedReturnType :: Maybe VarType
}

type Cont = Closure -> IO ()
type ContE = Value -> Cont
type ContD = Env -> Cont

data Value = ValVoid
           | ValInt { valToInt :: Int }
           | ValChar { valToChar :: Char }
           | ValRef { valToRef :: Location }
           | ValObject { valObjAttrs :: AttrDict }

type AttrDict = M.Map VarName Location

data VarType = RefVar ClassNameDesc
             | ValVar StructName

data ClassDesc = ClassDesc {
    classProps :: M.Map VarName PropDesc,
    classMths :: M.Map String MthDesc
}

data PropDesc = PropDesc {
    propGetType :: Maybe VarType
    propSetType :: Maybe VarType
}

data MthDesc = MthDesc {
    mthRetType :: VarType,
    mthArgs :: [(VarName, VarType, Maybe Location)]
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
    envClasses=M.fromList [
            ("Type", classType)
        ],
    envStructs=M.fromList [
            ("Type", structType)
        ],
    envExpectedReturnType=Nothing
} where
    classType = ClassDesc {
        classProps=M.fromList [],
        classMths=M.fromList []
    }
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

moduleSem :: Module -> SemanticErrorOr IO ()
stmtBlockSem :: StatementBlock -> Env -> SemanticErrorOr (Cont -> Cont)
stmtSem :: Stmt -> Env -> SemanticErrorOr (Cont -> Cont)
exprSem :: Expr -> Env -> SemanticErrorOr (ContE -> Cont)
declSem :: Decl -> Env -> SemanticErrorOr (ContE -> Cont)

type SemanticErrorOr s = Either SemanticError s

data SemanticError = SemanticError String

instance Error SemanticError where
  strMsg = SemanticError

showSemError :: SemanticError -> String
showSemError (SemanticError s) = "SemanticError: " ++ s

