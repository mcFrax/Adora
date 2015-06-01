module Memory where

import qualified Data.Map.Strict as M

import Types

infixl 9 !!!
(!!!) :: (Ord k, Show k, Show a) => M.Map k a -> k -> a
m !!! kk = do
    case M.lookup kk m of
        Just r -> r
        Nothing -> error ("!!!: given key is not an element in the map:\n" ++
                          (show kk) ++ "\n" ++ (show m))

memFrame :: Memory -> Frame
memFrame mem = (memFrames mem) !!! (memFid mem)

memObjAt :: Memory -> MemPt -> Object
memObjAt mem pt = (memObjects mem) !!! pt

memAdjust :: Memory -> MemPt -> (Object -> Object) -> Memory
memAdjust mem pt f = mem{memObjects=M.adjust f pt (memObjects mem)}

setFid :: Fid -> Memory -> Memory
setFid fid mem = mem{memFid=fid}

allocObject :: Object -> Memory -> (MemPt, Memory)
allocObject obj mem = do
    let (pt, values) = insertNext obj $ memObjects mem
    (pt, mem{memObjects=values})

allocFrame :: Fid -> Memory -> (Fid, Memory)
allocFrame closureFid mem = do
    let frame = Frame{
        frameParentId=closureFid,
        frameContent=M.empty
    }
    let (fid, frames) = insertNext frame $ memFrames mem
    (fid, mem{memFrames=frames})

getVar :: FrameKey -> Memory -> VarVal
getVar k mem = do
    getVar' $ memFrame mem
    where
        frames = memFrames mem
        getVar' f = do
            case M.lookup k (frameContent f) of
                Just val -> val
                Nothing -> getVar' (frames !!! (frameParentId f))

allocVar :: FrameKey -> VarVal -> Memory -> Memory
allocVar k pt mem =
    allocFrameVar (memFid mem) k pt mem

allocFrameVar :: Fid -> FrameKey -> VarVal -> Memory -> Memory
allocFrameVar fid k pt mem = do
    let frames = memFrames mem
    let frame = frames !!! fid
    let frame' = frame{frameContent=M.insert k pt $ frameContent frame}
    mem{memFrames=M.insert fid frame' frames}

assignVar :: FrameKey -> VarVal -> Memory -> Memory
assignVar k val mem = do
    getVarPt' $ memFid mem
    where
        frames = memFrames mem
        getVarPt' fid = let
            frame = frames !!! fid
            in case M.lookup k (frameContent frame) of
                Just _ -> let
                    frame' = frame{frameContent=M.insert k val $ frameContent frame}
                    in mem{memFrames=M.insert fid frame' frames}
                Nothing -> getVarPt' $ frameParentId frame

class Ord k => MemKey k where
    firstKey :: k
    keySuccessor :: k -> k

instance MemKey MemPt where
    firstKey = MemPt 0
    keySuccessor (MemPt pt) = MemPt $ pt + 1

instance MemKey Fid where
    firstKey = Fid 0
    keySuccessor (Fid pt) = Fid $ pt + 1

insertNext :: MemKey k => a -> M.Map k a -> (k, M.Map k a)
insertNext v m = let
    key = nextKey m
    in (key, M.insert key v m)

nextKey :: MemKey k => M.Map k a -> k
nextKey m = do
    if M.null m
        then
            firstKey
        else keySuccessor $ fst $ M.findMax m
