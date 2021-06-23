{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables, RankNTypes #-}
{-# Language OverloadedStrings #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveFunctor #-}
--{-# Language TemplateHaskell #-}

module Quern.Render.StaticScene.SceneStorage where

--
--import Control.Concurrent.STM
import Control.Lens ((^.))
import Control.Monad (when, forM)
import Control.Monad.IO.Class
import Data.Int
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T (Text)
import qualified Data.Vector.Storable as VS
--import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word
import Foreign hiding (rotate)
import GHC.Generics (Generic)
import Graphics.GL.Core45
--import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.GL.Types
import Linear
import Numeric.Half

import Quern.Render.GL
import Quern.Render.Instance
import Quern.Render.Internal.StorageBuffer
--import Quern.Render.Target

import Quern.Logging
import Quern.Render.Opacity
--import Quern.Util

-- Pair of a mesh filename/descriptor and a material name
newtype MeshKey = MeshKey (FilePath, String) deriving (Eq, Ord, Show, Read)



intToPtr :: Integral i => i -> Ptr a
intToPtr = intPtrToPtr . fromIntegral

-- TODO: split into streams (shadows, compute...)
-- | Fixed format for static vertices
data StaticVertex = StaticVertex
  { _vertexPos :: !(V3 Float)  -- 12
  , _vertexColour :: !(V4 Word8) -- 4
  , _vertexTangentFrame :: !(V2 (V4 Int8)) -- 8
  , _vertexUV0 :: !(V2 Half) -- 4
  , _vertexUV1 :: !(V2 Half) -- 4
  } deriving (Eq, Ord, Show, Read, Generic)

vertexNormal :: StaticVertex -> V3 Float
vertexNormal vtx = if sg < 0 then negate n else n
  where
    (V2 qt@(V4 _ _ _ sg) qb) = _vertexTangentFrame vtx
    t = ((/127) . fromIntegral <$> qt) ^. _xyz
    b = ((/127) . fromIntegral <$> qb) ^. _xyz
    n = normalize (t `cross` b)

staticVertexSize, staticIndexSize :: Int
staticVertexSize = sizeOf (undefined :: StaticVertex)
staticIndexSize = sizeOf (undefined :: Word32)

-- using a named VAO
staticVertexFormat :: MonadIO m => GLuint -> m ()
staticVertexFormat vao = do
  flip mapM_ [0..5] $ \i -> do
    glEnableVertexArrayAttrib vao i
    glVertexArrayAttribBinding vao i 0
  glVertexArrayAttribFormat vao 0 3 GL_FLOAT GL_FALSE 0
  glVertexArrayAttribFormat vao 1 4 GL_UNSIGNED_BYTE GL_TRUE 12
  glVertexArrayAttribFormat vao 2 4 GL_BYTE GL_TRUE 16
  glVertexArrayAttribFormat vao 3 4 GL_BYTE GL_TRUE 20
  glVertexArrayAttribFormat vao 4 2 GL_HALF_FLOAT GL_FALSE 24
  glVertexArrayAttribFormat vao 5 2 GL_HALF_FLOAT GL_FALSE 28

-- if the VAO is already bound...
staticVertexFormatBound :: MonadIO m => m ()
staticVertexFormatBound  = do
  flip mapM_ [0..5] $ \i -> do
    glEnableVertexAttribArray  i
    glVertexAttribBinding  i 0
  glVertexAttribFormat 0 3 GL_FLOAT GL_FALSE 0
  glVertexAttribFormat 1 4 GL_UNSIGNED_BYTE GL_TRUE 12
  glVertexAttribFormat 2 4 GL_BYTE GL_TRUE 16
  glVertexAttribFormat 3 4 GL_BYTE GL_TRUE 20
  glVertexAttribFormat 4 2 GL_HALF_FLOAT GL_FALSE 24
  glVertexAttribFormat 5 2 GL_HALF_FLOAT GL_FALSE 28

emptyVert :: StaticVertex
emptyVert = StaticVertex 0 255 (V2 (V4 127 0 0 127) (V4 0 127 0 0)) 0 0

instance Storable StaticVertex where
  sizeOf _ = 32
  alignment _ = 1
  peek a = do
    pos <- peek (castPtr a)
    clr <- peek (a `plusPtr` 12)
    rot <- peek (a `plusPtr` 16)
    uv0 <- peek (a `plusPtr` 24)
    uv1 <- peek (a `plusPtr` 28)
    pure $ StaticVertex pos clr rot uv0 uv1
  poke a (StaticVertex pos clr rot uv0 uv1) = do
    poke (castPtr a) pos
    poke (a `plusPtr` 12) clr
    poke (a `plusPtr` 16) rot
    poke (a `plusPtr` 24) uv0
    poke (a `plusPtr` 28) uv1
    pure ()


type MaterialKey =  String
newtype MaterialIndex = Mtl Int deriving (Eq, Ord, Show)

-- | Materials are collections of texture handles
data Material a = Material
  { _materialBaseColour :: !a
  , _materialNormal :: !a
  , _materialAmbientRoughnessMetallicHeight :: !a
  , _materialSubsurfaceEmissive :: !a
  } deriving (Eq, Ord, Show, Functor, Generic)

data MtlData
  = MtlTexture FilePath
  | MtlColour (V4 Word8)
  | MtlChannels (V4 (Either FilePath Word8))
  deriving (Eq, Ord, Show, Read, Generic)

instance Applicative Material where
  pure x = Material x x x x
  (Material f g h i) <*> (Material x y z w) = Material (f x) (g y) (h z) (i w)

instance Foldable Material where
  foldMap f (Material x y z w) = f x <> f y <> f z <> f w

instance Traversable Material where
  traverse f (Material x y z w) = Material <$> f x <*> f y <*> f z <*> f w

type MaterialHandles = Material GLuint64
type MaterialPaths = Material FilePath
type MaterialMesh = Material MtlData
type MaterialFlat = Material (V4 Word8)
type MaterialPathsFallback = Material (FilePath, V4 Word8)

defaultMaterialColours :: MaterialFlat
defaultMaterialColours = Material
  (V4 128 128 128 255)
  (V4 128 128 255 0)
  (V4 255 240 0 128)
  (V4 0 0 0 0)

defaultMaterialExt :: Material T.Text
defaultMaterialExt = Material
  "_baseColour"
  "_normal"
  "_amrh"
  "_subs"

textureHandleSize :: Int
textureHandleSize = sizeOf (0 :: GLuint64)

instance Storable MaterialHandles where
  sizeOf _ = textureHandleSize * 4
  alignment _ = 1
  peek ptr = do
    bse <- peek (castPtr ptr)
    nrm <- peek (ptr `plusPtr` (textureHandleSize * 1))
    arm <- peek (ptr `plusPtr` (textureHandleSize * 2))
    sub <- peek (ptr `plusPtr` (textureHandleSize * 3))
    pure $ Material bse nrm arm sub
  poke ptr (Material bse nrm arm sub) = do
    poke (castPtr ptr) bse
    poke (ptr `plusPtr` (textureHandleSize * 1)) nrm
    poke (ptr `plusPtr` (textureHandleSize * 2)) arm
    poke (ptr `plusPtr` (textureHandleSize * 3)) sub

data MaterialStorage = MaterialStorage
  { _materialStore :: !(StorageBuffer MaterialHandles)
  , _materialStoreLookup :: !(IORef (Map MaterialKey MaterialIndex)) -- flat assoc vector?
  }

maxMaterial :: Int
maxMaterial = 65536

materialStorageNew :: MonadIO m => m MaterialStorage
materialStorageNew = MaterialStorage <$> storageNew GL_SHADER_STORAGE_BUFFER maxMaterial <*> liftIO (newIORef mempty)

materialLookup :: MonadIO m => MaterialStorage -> MaterialKey -> m (Maybe MaterialIndex)
materialLookup store key =
  liftIO $ M.lookup key <$> readIORef (_materialStoreLookup store)

materialStore :: MonadIO m => MaterialStorage -> MaterialKey -> MaterialHandles -> m MaterialIndex
materialStore store key material = do
  exists <- liftIO $ M.lookup key <$> readIORef (_materialStoreLookup store)
  case exists of
    Just ix ->
      pure ix
    Nothing -> liftIO $ do
      ix <- Mtl <$> with material (unsafeStore (_materialStore store) 1)
      modifyIORef' (_materialStoreLookup store) (M.insert key ix)
      pure ix

--materialLoad :: MonadIO m => MaterialStorage -> MaterialKey -> MaterialPaths -> (FilePath -> m )

newtype InstanceIndex = Inst Int deriving (Eq, Ord, Show)

newtype InstanceStorage = InstanceStorage
  { _instanceStore :: (StorageBuffer Instance) -- max. 65536
--  , _instanceStoreLookup :: !(IORef (Map InstanceKey Int))
  }

maxInstance :: Int
maxInstance = 65536

instanceStorageNew :: MonadIO m => m InstanceStorage
instanceStorageNew = InstanceStorage <$> storageNew GL_SHADER_STORAGE_BUFFER maxInstance  -- <*> newIORef mempty

instanceStore :: MonadIO m => InstanceStorage -> Instance -> m InstanceIndex
instanceStore store inst = fmap Inst . liftIO $ with inst $ unsafeStore (_instanceStore store) 1

-- Mesh storage
data AABB = AABB { _aabbMin :: !(V4 Float), _aabbMax :: !(V4 Float) } deriving (Eq, Ord, Show, Read)
instance Semigroup AABB where
  (AABB mnA mxA) <> (AABB mnB mxB) = AABB (min <$> mnA <*> mnB) (max <$> mxA <*> mxB)

degenerate :: V3 Float -> AABB
degenerate (V3 x y z) = let v = V4 x y z 0 in AABB v v

expand :: V3 Float -> AABB -> AABB
expand (V3 x y z) (AABB mn mx) = let p = V4 x y z 0 in AABB (min <$> p <*> mn) (max <$> p <*> mx)

instance Storable AABB where
  sizeOf _ = sizeOf (0 :: Float) * 8
  alignment _ = 1
  peek ptr = AABB <$> peek (castPtr ptr) <*> peek (ptr `plusPtr` 16)
  poke ptr (AABB mn mx) = poke (castPtr ptr) mn *> poke (ptr `plusPtr` 16) mx

aabbSize :: Int
aabbSize = sizeOf (AABB 0 0)


-- TODO: rename this little structure
data MeshSlice = MeshSlice
  { _meshSliceIndexStart :: !Int
  , _meshSliceIndexCount :: !Int
  , _meshSliceDataIndex :: !Int
  , _meshSliceDefaultMtl :: !MaterialIndex
  , _meshSliceDefaultOpacity :: !Opacity
  --, _meshSliceAABB :: !AABB
  } deriving (Eq, Ord, Show)

data MeshLOD = MeshLOD
  { _meshLODElementCount :: !GLuint
  , _meshLODFirstIndex :: !GLuint
  } deriving (Eq, Ord, Show)

instance Storable MeshLOD where
  sizeOf _ = sizeOf (0 :: GLuint) * 2
  alignment _ = 1
  peek ptr = MeshLOD <$> peek (castPtr ptr) <*> peek (ptr `plusPtr` 4)
  poke ptr (MeshLOD cnt idx) = poke (castPtr ptr) cnt *> poke (ptr `plusPtr` 4) idx

data MeshData = MeshData
  { _meshDataAABB :: !AABB
  , _meshDataLOD0 :: !MeshLOD
  } deriving (Eq, Ord, Show)

instance Storable MeshData where
  sizeOf _ = sizeOf (undefined :: MeshLOD) + sizeOf (undefined :: AABB)
  alignment _ = 1
  peek ptr = MeshData <$> peek (castPtr ptr) <*> peek (ptr `plusPtr` 32)
  poke ptr (MeshData aabb lod0) = poke (castPtr ptr) aabb *> poke (ptr `plusPtr` 32) lod0

meshDataSize :: Int
meshDataSize = sizeOf (undefined :: MeshData)

-- no ability to free: compaction problems (index fixup)
-- | Meshes are just vertex and index buffers plus a VAO with associated
-- vertex layout and such
data StaticMeshStorage = StaticMeshStorage
  { _meshStoreVertex :: !(StorageBuffer StaticVertex)
  , _meshStoreIndex :: !(StorageBuffer Word32)
  , _meshStoreMeshData :: !(StorageBuffer AABB)
  , _meshStoreVAObject :: !GLuint
  , _meshStoreBuffers :: !(IORef (Map FilePath (Map String MeshSlice)))
  }

meshStorageReport :: MonadIO m => StaticMeshStorage -> m [(MeshKey, MeshSlice)]
meshStorageReport storage = liftIO $ do
  ms <- M.toList <$> readIORef (_meshStoreBuffers storage)
  let go (fp, slices) = fmap (\(mtl, slice) -> (MeshKey (fp, mtl), slice)) (M.toList slices)
  pure $ concatMap go ms

meshStorageNew :: MonadIO m => m StaticMeshStorage
meshStorageNew = do
    vtx <- storageNew GL_ARRAY_BUFFER vtxCapacity --staticVertexSize
    idx <- storageNew GL_ELEMENT_ARRAY_BUFFER idxCapacity --staticIndexSize
    mdata <- storageNew GL_SHADER_STORAGE_BUFFER dataCapacity --aabbSize --meshDataSize
    buffers <- liftIO $ newIORef mempty
    --glVAO <- liftIO $ alloca $ \ptr -> glGenVertexArrays 1 ptr *> peek ptr
    glVAO <- allocateObject (glGenVertexArrays 1)
    -- this named path triggers GL_INVALID_OPERATION; why?
    --staticVertexFormat glVAO
    --glVertexArrayVertexBuffer glVAO 0 (_storeObject vtx) 0 vtxStride
    --glVertexArrayElementBuffer glVAO (_storeObject idx)
    glBindVertexArray glVAO
    staticVertexFormatBound
    glBindVertexBuffer 0 (_storeObject vtx) 0 vtxStride
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER (_storeObject idx)
    glBindVertexArray 0
    pure $! StaticMeshStorage {
        _meshStoreVertex = vtx
      , _meshStoreIndex = idx
      , _meshStoreVAObject = glVAO
      , _meshStoreBuffers = buffers
      , _meshStoreMeshData = mdata
      }
  where
    vtxCapacity = 1024*1024*8
    idxCapacity = vtxCapacity*6
    dataCapacity = 65536
    vtxStride = fromIntegral staticVertexSize

fetchMeshSlices :: (MonadIO m)
                => StaticMeshStorage -> FilePath -> m (Maybe [(String, MeshSlice)])
fetchMeshSlices storage fp = liftIO $ fmap M.toList . M.lookup fp <$> readIORef (_meshStoreBuffers storage)

fetchMeshSlice :: (MonadIO m)
               => StaticMeshStorage -> MeshKey -> m (Maybe MeshSlice)
fetchMeshSlice storage (MeshKey (fp, mtl)) =
  liftIO $ (\m -> M.lookup fp m >>= M.lookup mtl) <$> readIORef (_meshStoreBuffers storage)

storeMeshSlice :: (HasLogging env, MonadReader env m, MonadIO m)
               => StaticMeshStorage -> MeshKey -> MaterialIndex -> Opacity -> VS.Vector StaticVertex -> VS.Vector Word32 -> m (Maybe MeshSlice)
storeMeshSlice storage (MeshKey (fp, mtl)) dfltMtl dfltOpacity vertices indices = do
    presentAlready <- liftIO $ do
      m <- M.lookup fp <$> readIORef (_meshStoreBuffers storage)
      pure $ m >>= M.lookup mtl
    case presentAlready of
      slice@(Just _) -> pure slice
      Nothing -> do
        ms <- storeMeshes storage fp vertices [(mtl, dfltMtl, dfltOpacity, indices)]
        case ms of
          [] -> pure Nothing
          (Just (_,m):_) -> pure (Just m)
          (Nothing:_) -> pure Nothing



storeMeshes ::(HasLogging env, MonadReader env m, MonadIO m)
            => StaticMeshStorage -> FilePath -> VS.Vector StaticVertex -> [(String, MaterialIndex, Opacity, VS.Vector Word32)] -> m [Maybe (MeshKey, MeshSlice)]
storeMeshes storage meshPath vertices mtlKeyIndices
  | VS.null vertices || null mtlKeyIndices = loggingString ("Empty mesh: " <> meshPath <> " / " <> show ((\(n,_,_,_) -> n) <$> mtlKeyIndices)) *> pure []
  | otherwise = do
      freeVtx <- storageFreeSpace (_meshStoreVertex storage)
      offset <- fromIntegral <$> storageUsedSpace (_meshStoreVertex storage)
      let vtxCount = VS.length vertices
      if freeVtx < vtxCount
        then loggingString "No space in vertex buffer!" *> pure []
        else do
          glErrorToLog "storeMesh.beforeStore"
          liftIO $ VS.unsafeWith vertices $ \ptr ->
            unsafeStore (_meshStoreVertex storage) vtxCount ptr *> pure ()
          glErrorToLog "storeMesh.storeVertices"
          is <- forM mtlKeyIndices $ \(mtlKey, dfltMtl, dfltOpacity, indices) ->
                  if VS.null indices
                    then pure Nothing
                    else do
                      let indices' = VS.map (+ offset) indices
                          idxCount = VS.length indices
                      idxOff <- storageUsedSpace (_meshStoreIndex storage)
                      liftIO $ VS.unsafeWith indices' $ \ptr ->
                        unsafeStore (_meshStoreIndex storage) idxCount ptr *> pure ()
                      glErrorToLog "storeMesh.storeIndices"
                      let pos0 = _vertexPos (vertices VS.! fromIntegral (VS.head indices))
                          aabb = VS.foldl' (\bb ix -> expand (_vertexPos (vertices VS.! fromIntegral ix)) bb) (degenerate pos0) indices
                          --mdata = MeshData aabb (MeshLOD (fromIntegral idxCount) (fromIntegral idxOff))
                      mdataIdx <- liftIO $ with aabb $ unsafeStore (_meshStoreMeshData storage) 1
                      glErrorToLog "storeMesh.storeMeshData"
                      let buf = MeshSlice
                                  { _meshSliceIndexStart = idxOff
                                  , _meshSliceIndexCount = idxCount
                                  , _meshSliceDataIndex = mdataIdx
                                  , _meshSliceDefaultMtl = dfltMtl
                                  , _meshSliceDefaultOpacity = dfltOpacity
                                  }
                          nestedInsert m = case M.lookup meshPath m of
                            Just m' -> M.insert meshPath (M.insert mtlKey buf m') m
                            Nothing -> M.insert meshPath (M.singleton mtlKey buf) m
                      liftIO $ modifyIORef' (_meshStoreBuffers storage) nestedInsert
                      pure $! Just $! (MeshKey (meshPath, mtlKey), buf)
          pure is

{-


          freeIdx <- storageFreeSpace (_meshStoreIndex storage)
          freeMsh <- storageFreeSpace (_meshStoreMeshData storage)
          let indicesValid = VS.all (< vtxCount') indices
           || freeIdx < idxCount || not indicesValid || freeMsh < 1
            then do
              loggingString $! unlines
                [ "Invalid mesh data!"
                , "  Vertices: " ++ show vtxCount ++ " / " ++ show freeVtx
                , "   Indices: " ++ show idxCount ++ " / " ++ show freeIdx
                , "  MeshData: " ++ show freeMsh
                , "  Idx sane: " ++ show indicesValid
                ]
              pure Nothing
            else do
              idxOff <- storageUsedSpace (_meshStoreIndex storage)
              mshOff <- storageUsedSpace (_meshStoreMeshData storage)
              loggingString $ "Storing new mesh at: vertex " ++ show vtxOff ++ ", index " ++ show idxOff ++ ", mesh " ++ show mshOff
              loggingString $ unwords ["Free space: vertex", show freeVtx, "index", show freeIdx, "mesh", show freeMsh]
              let indices' = VS.map (+ offset) indices

              glErrorToLog "storeMesh.beforeStore"
              glErrorToLog "storeMesh.storeIndices"
              let vtxPos0 = _vertexPos (vertices VS.! 0)
                  aabb = VS.foldl' (\bb vtx -> expand (_vertexPos vtx) bb) (degenerate vtxPos0) vertices
                  mdata = MeshData aabb (MeshLOD (fromIntegral idxCount) (fromIntegral idxOff))
              --mdataIdx <- with mdata $ \ptr -> unsafeStore (_meshStoreMeshData storage) ptr 1
              mdataIdx <- liftIO $ with aabb $  unsafeStore (_meshStoreMeshData storage) 1
              glErrorToLog "storeMesh.storeMeshData"
              loggingString $ "Storing mesh data [" ++ show mdataIdx ++ "]:\n\t\t" ++ show mdata
              let buf = MeshSlice { _meshSliceIndexStart = idxOff, _meshSliceIndexCount = idxCount, _meshSliceDataIndex = mdataIdx }
              loggingString $ "Storing slice " ++ show buf
              liftIO $ modifyIORef' (_meshStoreBuffers storage) (M.insert key buf)
              pure $! Just $! buf
  where
    vtxCount = VS.length vertices
    idxCount = VS.length indices
    vtxCount' = fromIntegral vtxCount
-}


-- can be stored in buffer bound to GL_DRAW_INDIRECT_BUFFER
data IndirectDraw = IndirectDraw
  { _indirectCount :: !GLuint -- element count (triangles?)
  , _indirectInstances :: !GLuint
  , _indirectFirstIndex :: !GLuint
  , _indirectBaseVertex :: !GLuint
  , _indirectBaseInstance :: !GLuint -- encode material and matrix here
  } deriving (Eq, Ord, Show)

indirectMemberSize :: Int
indirectMemberSize = sizeOf (0 :: GLuint)

instance Storable IndirectDraw where
  sizeOf _ = indirectMemberSize * 5
  alignment _ = 1
  peek ptr = do
    count <- peek (castPtr ptr)
    insts <- peek (ptr `plusPtr` (indirectMemberSize * 1))
    fstix <- peek (ptr `plusPtr` (indirectMemberSize * 2))
    basev <- peek (ptr `plusPtr` (indirectMemberSize * 3))
    basei <- peek (ptr `plusPtr` (indirectMemberSize * 4))
    pure $ IndirectDraw count insts fstix basev basei
  poke ptr (IndirectDraw count insts fstix basev basei) = do
    poke (castPtr ptr) count
    poke (ptr `plusPtr` (indirectMemberSize * 1)) insts
    poke (ptr `plusPtr` (indirectMemberSize * 2)) fstix
    poke (ptr `plusPtr` (indirectMemberSize * 3)) basev
    poke (ptr `plusPtr` (indirectMemberSize * 4)) basei

data StaticIndirectDrawStorage = StaticIndirectDrawStorage
  { _indirectDraws :: (StorageBuffer IndirectDraw)
  , _packedDraws :: (StorageBuffer PackedDraw)
  , _shadowDraws :: (StorageBuffer IndirectDraw)
  , _opacityDraws :: !Opacity
  }

indirectStorageClear :: MonadIO m => StaticIndirectDrawStorage -> m ()
indirectStorageClear s = do
  storageClear (_indirectDraws s)
  storageClear (_packedDraws s)
  storageClear (_shadowDraws s)

indirectStorageNew :: MonadIO m => Opacity -> m StaticIndirectDrawStorage
indirectStorageNew opacity = do
    indirect <- storageNew GL_DRAW_INDIRECT_BUFFER cap
    packed <- storageNew GL_SHADER_STORAGE_BUFFER cap
    shadow <- storageNew GL_DRAW_INDIRECT_BUFFER cap
    pure $! StaticIndirectDrawStorage indirect packed shadow opacity
  where
    cap = 65536

data DrawIndex = Draw !Int !Opacity deriving (Eq, Ord, Show)

indirectDrawStore :: (HasLogging env, MonadReader env m, MonadIO m) => StaticIndirectDrawStorage -> MeshSlice -> MaterialIndex -> InstanceIndex -> m DrawIndex
indirectDrawStore (StaticIndirectDrawStorage indirect packed shadow opacity) (MeshSlice idxStart idxCount idxData _ _) (Mtl mtl) (Inst inst)
  | mtl >= maxMaterial || inst >= maxInstance || mtl < 0 || inst < 0 = logging "Invalid material/instance index" *> pure (Draw (-1) Opaque)
  | otherwise = do
      packIx <- liftIO $ with pack $ unsafeStore packed   1
      indrIx <- liftIO $ with draw $ unsafeStore indirect 1
      _      <- liftIO $ with draw $ unsafeStore shadow   1
      when (packIx /= indrIx) (loggingString $ "mismatched packed/indirect draw indices! " ++ show packIx ++ " /= " ++ show indrIx)
      pure (Draw indrIx opacity)
  where
    draw = IndirectDraw (fromIntegral idxCount) 1 (fromIntegral idxStart) 0 mtlInst
    pack = PackedDraw (fromIntegral idxData) (fromIntegral mtl) (fromIntegral inst)
    mtlInst = fromIntegral $ (mtl `shiftL` 16) .|. inst

-- | Can be used by a compute shader to generate indirect draws
data PackedDraw = PackedDraw
  { _packedDrawMeshIndex :: !GLuint
  , _packedDrawMaterialIndex :: !GLuint
  , _packedDrawInstanceIndex :: !GLuint
  } deriving (Eq, Ord, Show)

packedDrawMemberSize :: Int
packedDrawMemberSize = sizeOf (0 :: GLuint)

instance Storable PackedDraw where
  sizeOf _ = packedDrawMemberSize * 3
  alignment _ = 1
  peek ptr = do
    msh  <- peek (ptr `plusPtr` (packedDrawMemberSize*0))
    mtl  <- peek (ptr `plusPtr` (packedDrawMemberSize*1))
    inst <- peek (ptr `plusPtr` (packedDrawMemberSize*2))
    pure $! PackedDraw msh mtl inst
  poke ptr (PackedDraw msh mtl inst) = do
    poke (castPtr ptr) msh
    poke (ptr `plusPtr` (packedDrawMemberSize*1)) mtl
    poke (ptr `plusPtr` (packedDrawMemberSize*2)) inst

--
makeQuad :: V2 Float -> V2 Half -> VS.Vector StaticVertex
makeQuad (V2 x y) (V2 u v) = VS.fromList
    [ emptyVert{ _vertexColour = 0xff, _vertexPos = V3 0 0 0, _vertexUV0 = V2 0 0 }
    , emptyVert{ _vertexColour = 0xff, _vertexPos = V3 x 0 0, _vertexUV0 = V2 u 0 }
    , emptyVert{ _vertexColour = 0xff, _vertexPos = V3 0 y 0, _vertexUV0 = V2 0 v }
    , emptyVert{ _vertexColour = 0xff, _vertexPos = V3 x y 0, _vertexUV0 = V2 u v }
    ]

quadIndices :: VS.Vector Word32
quadIndices = VS.fromList [ 0, 1, 2, 2, 1, 3 ]

-- debug meshes
{-
storeDebugQuad storage logging =
  storeMesh storage "debug_quad" quadVerts quadIndices logging

storeDebugCube storage logging =
  storeMesh storage "debug_cube" cubeVerts cubeIndices logging

quadVerts :: VS.Vector StaticVertex
quadVerts = VS.fromList
    [ emptyVert{ _vertexColour = V4 0xff 0x00 0x00 0xff, _vertexPos = V3 n n 0, _vertexUV0 = V2 0 0 }
    , emptyVert{ _vertexColour = V4 0x00 0xff 0x00 0xff, _vertexPos = V3 p n 0, _vertexUV0 = V2 1 0 }
    , emptyVert{ _vertexColour = V4 0x00 0x00 0xff 0xff, _vertexPos = V3 n p 0, _vertexUV0 = V2 0 1 }
    , emptyVert{ _vertexColour = V4 0xff 0xff 0xff 0xff, _vertexPos = V3 p p 0, _vertexUV0 = V2 1 1 }
    ]
  where
    p = 0.5
    n = negate p


cubeVerts :: VS.Vector StaticVertex
cubeVerts = VS.fromList
  [ StaticVertex {_vertexPos = V3 posN posP posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 127 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 0.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posP posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 127 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 1.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posN posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 127 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 1.0 (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posN posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 127 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 0.0 (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posP posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 0.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posP posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 1.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posN posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 1.0 (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posN posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 (-127) 0 0), _vertexUV0 = V2 0.0 (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posP posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 (-127) 127) (V4 0 127 0 0), _vertexUV0 = V2 0.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posP posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 (-127) 127) (V4 0 127 0 0), _vertexUV0 = V2 (-1.0) 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posN posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 (-127) 127) (V4 0 127 0 0), _vertexUV0 = V2 (-1.0) (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posN posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 0 0 (-127) 127) (V4 0 127 0 0), _vertexUV0 = V2 0.0 (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posP posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 127 0 0), _vertexUV0 = V2 0.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posP posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 127 0 0), _vertexUV0 = V2 (-1.0) 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posN posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 127 0 0), _vertexUV0 = V2 (-1.0) (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posN posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 127 0 0), _vertexUV0 = V2 0.0 (-1.0), _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posN posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 0 127 0), _vertexUV0 = V2 0.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posN posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 0 127 0), _vertexUV0 = V2 0.0 1.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posN posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 0 127 0), _vertexUV0 = V2 1.0 1.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posN posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 (-127) 0 0 (-127)) (V4 0 0 127 0), _vertexUV0 = V2 1.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posP posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 0 (-127) 0), _vertexUV0 = V2 0.0 0.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posP posP posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 0 (-127) 0), _vertexUV0 = V2 0.0 1.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posP posN, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 0 (-127) 0), _vertexUV0 = V2 (-1.0) 1.0, _vertexUV1 = V2 0.0 0.0}
  , StaticVertex {_vertexPos = V3 posN posP posP, _vertexColour = clr, _vertexTangentFrame = V2 (V4 127 0 0 127) (V4 0 0 (-127) 0), _vertexUV0 = V2 (-1.0) 0.0, _vertexUV1 = V2 0.0 0.0}
  ]
  where
    clr = V4 0xff 0xff 0xff 0xff
    posN = -0.5
    posP = 0.5

cubeIndices :: VS.Vector Word32
cubeIndices = VS.fromList [0,1,2,0,2,3,4,5,6,4,6,7,8,9,10,8,10,11,12,13,14,12,14,15,16,17,18,16,18,19,20,21,22,20,22,23]
-}
