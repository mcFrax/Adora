module ExprExe where

import Memory
import Types

rValue :: ExprSem -> SemiCont Value
rValue = execV.expRValue

rValuePt :: ExprSem -> SemiCont Pointer
rValuePt = execPt.expRValue

execV :: Either (Exe Value) (Exe Pointer) -> SemiCont Value
execV (Left exeVal) = exec exeVal
execV (Right exePt) = \ke -> do
    exec exePt $ \pt re mem -> do
        ke (memGet mem pt) re mem

execPt :: Either (Exe Value) (Exe Pointer) -> SemiCont Pointer
execPt (Left exeVal) = \kpt -> do
    exec exeVal $ \val re mem -> do
        let (pt, mem') = alloc val mem
        kpt pt re mem'
execPt (Right exePt) = exec exePt

mkExeV :: SemiCont Value -> Either (Exe Value) (Exe Pointer)
mkExeV = (Left).mkExe

mkExePt :: SemiCont Pointer -> Either (Exe Value) (Exe Pointer)
mkExePt = (Right).mkExe

isTruthy :: Value -> Bool
isTruthy = valToBool
-- TODO: isTruthy for other values
