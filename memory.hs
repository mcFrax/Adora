module Memory where

import Types

memFrame :: Memory -> Frame
memFrame mem = (memFrames mem) M.! (memFid mem)

memGet :: Memory -> Pointer -> Value
memGet mem pt = (memValues mem) M.! pt

memSet :: Memory -> Pointer -> Value -> Memory
memSet mem pt v = mem{memValues=M.insert pt v (memValues mem)}

setFid :: Fid -> Memory -> Memory
setFid fid mem = mem{memFid=fid}

alloc :: Value -> Memory -> (Pointer, Memory)
alloc v mem = do
    let pt = memNext mem
    (pt, mem{
        memNext=(pt + 1),
        memValues=M.insert pt v (memValues mem)
    })

allocFrame :: Memory -> (Fid, Memory)
allocFrame mem = do
    let frame = Frame{
        frameParentId=memFid mem,
        frameContent=M.empty
    }
    let fid = memNextFid mem
    (fid, mem{
        memNextFid=(fid+1),
        memFrames=M.insert fid frame $ memFrames mem
    })

-- frameGet :: Frame -> FrameKey -> Memory -> Value
-- frameGet f k mem = memGet mem $ frameGetPt f k
--
-- frameGetPt :: Frame -> FrameKey -> Pointer
-- frameGetPt f k = (frameContent f) M.! k

getVarPt :: FrameKey -> Memory -> Pointer
getVarPt k mem = do
    getVarPt' $ memFrame mem
    where
        frames = memFrames mem
        getVarPt' f = do
            case M.lookup k (frameContent f) of
                Just pt -> pt
                Nothing -> getVarPt' (frames M.! (frameParentId f))

getVar :: FrameKey -> Memory -> Value
getVar k mem = memGet mem $ getVarPt k mem

allocVar :: FrameKey -> Value -> Memory -> Memory
allocVar k v mem = do
    allocVarFid (memFid mem) k v mem

allocVarFid :: Fid -> FrameKey -> Value -> Memory -> Memory
allocVarFid fid k v mem = do
    let (pt, mem') = alloc v mem
    let frames = memFrames mem
    let frame = frames M.! fid
    let frame' = frame{frameContent=M.insert k pt $ frameContent frame}
    mem'{memFrames=M.insert fid frame' frames}

assignVar :: FrameKey -> Value -> Memory -> Memory
assignVar k v mem = do
    memSet mem (getVarPt k mem) v
