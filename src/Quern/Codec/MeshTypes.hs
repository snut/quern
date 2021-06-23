module Quern.Codec.MeshTypes where

-- shared types for loading obj and fbx files
import Control.Monad.ST
import Data.Foldable (traverse_, foldlM)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Int
import Data.Word
import Foreign
import Linear hiding (el)
import Numeric.Half

import Data.IORef

import Quern.Render.StaticScene.SceneStorage (StaticVertex(..))

-- after dumping everything else
data ObjVertex = ObjVertex
  { _objVertPos :: !(V3 Float)
  , _objVertNrm :: !(V3 Float)
  , _objVertTex :: !(V2 Float)
  , _objVertClr :: !(V4 Float)
  } deriving (Eq, Ord, Show, Read)

instance Storable ObjVertex where
  sizeOf _ = sizeOf (undefined :: Float) * 12
  alignment _ = 0
  peek ptr = do
    p <- peek (castPtr ptr)
    n <- peek (ptr `plusPtr` 12)
    t <- peek (ptr `plusPtr` 24)
    c <- peek (ptr `plusPtr` 32)
    pure $! ObjVertex p n t c
  poke ptr (ObjVertex p n t c) = do
    poke (castPtr ptr) p
    poke (ptr `plusPtr` 12) n
    poke (ptr `plusPtr` 24) t
    poke (ptr `plusPtr` 32) c

createTangents :: VS.Vector ObjVertex -> VS.Vector (V3 Word32) -> (VS.Vector (V3 Float), VS.Vector (V3 Float))
createTangents verts tris = runST $ do
  let n = VS.length verts
  bs <- VSM.replicate n 0
  ts <- VSM.replicate n 0
  VS.forM_ tris $ \tri -> do
    let itri = fromIntegral <$> tri
        V3 va vb vc = (verts VS.!) <$> itri
        V2 u_ba v_ba = _objVertTex vb - _objVertTex va
        V2 u_ca v_ca = _objVertTex vc - _objVertTex va
        edge_ba = _objVertPos vb - _objVertPos va
        edge_ca = _objVertPos vc - _objVertPos va
        t = (edge_ba ^* v_ca) - (edge_ca ^* v_ba)
        b = (edge_ca ^* u_ba) - (edge_ba ^* u_ca)
    traverse_ (VSM.modify ts (+ t)) itri
    traverse_ (VSM.modify bs (+ b)) itri
  flip mapM_ [0 .. n-1] $ \i -> do
    VSM.modify ts normalize i
    VSM.modify bs normalize i
  ts' <- VS.unsafeFreeze ts
  bs' <- VS.unsafeFreeze bs
  pure (ts', bs')

convertToStatic :: VS.Vector ObjVertex -> VS.Vector (V3 Word32) -> VS.Vector StaticVertex
convertToStatic vertices indices = VS.imap go vertices
  where
    (tangents, bitangents) = createTangents vertices indices
    go ix v = StaticVertex
      { _vertexPos = _objVertPos v
      , _vertexColour = quantiseW8 <$> _objVertClr v
      , _vertexTangentFrame = quantiseTangentFrame (_objVertNrm v) (tangents VS.! ix) (bitangents VS.! ix)
      , _vertexUV0 = toHalf <$> _objVertTex v
      , _vertexUV1 = 0
      }

quantiseTangentFrame :: V3 Float -> V3 Float -> V3 Float -> V2 (V4 Int8)
quantiseTangentFrame n t b = V2 (V4 tx ty tz tw) (V4 bx by bz 0)
  where
    -- allow for flipped normals
    n' = t' `cross` b'
    sg = signum (n `dot` n')
    -- ensure that the normal vector is recreated properly from the other parts
    -- will break if normal is aligned with generated tangents
    b' = b - (n ^* (n `dot` b))
    t' = t - (n ^* (n `dot` t))
    -- note that these are not normalised after quantisation, instead the max
    -- component is set to -127/127
    -- also note that they are permitted to be skewed
    V3 tx ty tz = quantiseI8 <$> (t' ^/ maximum (abs t'))
    V3 bx by bz = quantiseI8 <$> (b' ^/ maximum (abs b'))
    tw = if sg < 0 then -127 else 127

quantiseI8 :: Float -> Int8
quantiseI8 = truncate . min 127 . max (-127) . (* 127)

quantiseW8 :: Float -> Word8
quantiseW8 = truncate . min 255 . max 0 . (* 255)



-- small, unsafe little growing vector
data GVec a = GVec { _gvecData :: !(VSM.IOVector a), _gvecUsed :: {-# UNPACK #-} !Int }
newtype GrowVec a = GrowVec (IORef (GVec a))

newGrowVec :: Storable a => IO (GrowVec a)
newGrowVec = do
  vec <- VSM.unsafeNew 8
  GrowVec <$> newIORef (GVec vec 0)

unsafeFreeze :: Storable a => GrowVec a -> IO (VS.Vector a)
unsafeFreeze (GrowVec ref) = do
  GVec vec used <- readIORef ref
  let vec' = VSM.unsafeSlice 0 used vec
  VS.freeze vec'

incUsed :: GVec a -> GVec a
incUsed v@(GVec _ used) = v{ _gvecUsed = succ used }

pushVec :: Storable a => GrowVec a -> a -> IO Int
pushVec gvec@(GrowVec ref) el = do
  (GVec vec used) <- readIORef ref
  let cap = VSM.length vec
  if used + 1 < cap
    then VSM.write vec used el *> modifyIORef' ref incUsed *> pure used
    else do
      vec' <- VSM.grow vec (cap + 4)
      modifyIORef' ref (\gv -> gv{ _gvecData = vec' })
      pushVec gvec el

pushVec_ :: Storable a => GrowVec a -> a -> IO ()
pushVec_ v a = pushVec v a *> pure ()

usedVec :: GrowVec a -> IO Int
usedVec (GrowVec ref) = _gvecUsed <$> readIORef ref
