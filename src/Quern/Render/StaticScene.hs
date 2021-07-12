{-| Most of the rendering guts live here. -}

{-# Language OverloadedStrings  #-}
{-# Language TypeApplications #-}


module Quern.Render.StaticScene
  ( HasStaticScene(..)
  , newStaticScene
  , cullScene
  , drawScene
  , sunShadowScene
  , resizeSceneTargets
  , clearAllInstances
  , Opacity(..)
  , Multisampling(..)
  , reloadScenePrograms
  -- * Convenience
  , loadMaterial
  , loadMaterialFallback
  , loadMesh
  , loadSliMesh
  , loadFbxMesh
  , loadObjMesh
  , simpleMaterial
  , flatMaterial
  , mkInstance
  , drawInstance
  , drawInstanceDflt
  , updateInstanceSlow
  -- * Re-exports
  , Instance(..)
  , instanceTransform
  , instanceTint
  , instancePosition
  , instanceScale
  , instanceRotation
  , DrawIndex
  , MaterialIndex
  , InstanceIndex
  , MeshSlice
  , MaterialKey
  , MeshKey
  , module SceneTypes
  , meshStorageReport
  ) where


import Control.Lens (view, (^.), use, (.=), (%~), (&), (.~))
import Control.Monad (when, forM_)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.IORef
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM (HashMap, lookup, fromList, toList)
import qualified Data.Map as M (toList)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Numeric.Half (toHalf)
import System.Directory (doesFileExist)
import System.FilePath.Posix (takeFileName, takeExtension)
import Quern.Codec.Mtl (parseMtlFile, MaterialEx(..))
import Quern.Codec.Obj (parseObjFile, ObjMesh(..))
import Quern.Codec.FBX (fromFBXMeshFile, FBXMesh(..))
import Quern.Codec.SliMesh
import Quern.Compute.FrustumCulling
import Quern.Compute.PanoramaToCube
import Quern.Compute.PrecomputeBRDF
import Quern.Logging
import Quern.Render.Camera
import Quern.Render.GL
import Quern.Render.Instance
import Quern.Render.Internal.StorageBuffer
import Quern.Render.Opacity
import Quern.Render.Shader
import Quern.Render.StaticScene.SceneStorage
import Quern.Render.StaticScene.Types as SceneTypes
import Quern.Render.Target
import Quern.Render.Texture
import Quern.Types

class HasStaticScene a where
  staticScene :: Lens' a StaticScene

-- some convenience functions, helping to load/create some assets
toByte :: Float -> Word8
toByte = truncate . (* 255) . min 1 . max 0

-- | Generate a very simple opaque material given a base colour, roughness, and boolean metallic parameters
simpleMaterial :: (MonadIO m, MonadReader env m, HasTextureLibrary env, HasStaticScene env)
                   => MaterialKey -> V3 Float -> Float -> Bool -> m MaterialIndex
simpleMaterial key (V3 r g b) rough metal = do
  store <- view (staticScene . sceneMaterials)
  texLib <- view textureLibrary
  exists <- materialLookup store key
  case exists of
    Just ix -> pure ix
    Nothing -> do
      bc <- textureFromAction texLib (T.pack $ key <> "_baseColour") $ solidColourTexture2D'RGBA16F (V4 (toHalf r) (toHalf g) (toHalf b) 1) Resident
      nr <- textureFromAction texLib (T.pack "flatNormal") $ solidColourTexture2D (V4 128 128 255 255) Resident
      rm <- textureFromAction texLib (T.pack $ key <> "_roughMetal") $ solidColourTexture2D (V4 255 (toByte rough) (if metal then 255 else 0) 128) Resident
      sb <- textureFromAction texLib (T.pack "blackTransparent") $ solidColourTexture2D (pure 0) Resident
      let mtl = Material (_textureHandle bc) (_textureHandle nr) (_textureHandle rm) (_textureHandle sb)
      materialStore store key mtl

-- | Generate a material with where all textures are flat colour values
flatMaterial :: (MonadIO m, MonadReader env m, HasTextureLibrary env, HasStaticScene env)
             => MaterialKey -> MaterialFlat -> m MaterialIndex
flatMaterial key mtl = do
  store <- view (staticScene . sceneMaterials)
  texLib <- view textureLibrary
  exists <- materialLookup store key
  case exists of
    Just ix -> pure ix
    Nothing -> do
      let names = (T.pack key <>) <$> defaultMaterialExt :: Material T.Text
          texIO = flip solidColourTexture2D Resident <$> mtl :: MonadIO n => Material (n Texture)
      texs <- sequence $ textureFromAction texLib <$> names <*> texIO
      let mtlHdls = _textureHandle <$> texs :: Material GLuint64
      materialStore store key mtlHdls

-- | Generate a material by supplying paths to texture files
loadMaterial :: (MonadIO m, MonadReader env m, HasTextureLibrary env, HasStaticScene env)
             => MaterialKey -> MaterialPaths -> m MaterialIndex
loadMaterial key paths = do
  store <- view (staticScene . sceneMaterials)
  texLib <- view textureLibrary
  exists <- materialLookup store key
  case exists of
    Just ix -> pure ix
    Nothing -> do
      Material bc nr mh sb <- sequence $ textureFromFile'WithDefaultColour texLib <$> paths <*> pure Resident <*> defaultMaterialColours
      let mtl = Material (_textureHandle bc) (_textureHandle nr) (_textureHandle mh) (_textureHandle sb)
      materialStore store key mtl

loadMaterialFallback :: (MonadIO m, MonadReader env m, HasTextureLibrary env, HasStaticScene env, HasLogging env)
                     => MaterialKey -> MaterialPathsFallback -> m MaterialIndex
loadMaterialFallback key pathsFb = do
  store <- view (staticScene . sceneMaterials)
  texLib <- view textureLibrary
  exists <- materialLookup store key
  case exists of
    Just ix -> pure ix
    Nothing -> do
      loggingString $ unwords ["Loading Mtl:", key, show pathsFb]
      let loadT pth fb = textureFromFile'WithDefaultColour texLib pth Resident fb
      Material bc nr mh sb <- sequence $ (uncurry loadT) <$> pathsFb
      let mtl = Material (_textureHandle bc) (_textureHandle nr) (_textureHandle mh) (_textureHandle sb)
      materialStore store key mtl

replaceFilename :: FilePath -> FilePath -> FilePath
replaceFilename path newFile = dir ++ newFile
  where dir = reverse . dropWhile (\c -> c /= '/' && c /= '\\') . reverse $ path

-- A default material index is provided in case the material definition could not
-- be found, or the material prove invalid.
defaultMaterial :: MaterialIndex
defaultMaterial = Mtl 0

-- | Load a mesh from a file, attempting to also load any referenced materials.
-- Returns a list of pairs, each containing a `MaterialIndex` and a `MeshSlice`.
--
-- Note that mesh slices should all share an instance transform in order to reproduce
-- the entire mesh faithfully.
--
-- If the supplied path includes a valid extension (.obj, .fbx, .qmesh) that mesh loader is used
--
-- If the supplied path has no extension, the presence of .qmesh, .fbx and .obj files is checked in that order.
--
-- If the filepath has any other extension, the loading fails.
loadMesh :: (MonadIO m, MonadReader env m, HasLogging env, HasStaticScene env, HasTextureLibrary env)
         => FilePath -> m [MeshSlice]
loadMesh meshPath = case toLower <$> takeExtension meshPath of
  ".obj" -> loadObjMesh meshPath
  ".fbx" -> loadFbxMesh meshPath
  ".qmesh" -> loadSliMesh meshPath
  "" -> do
    let sliMesh = meshPath <> ".qmesh"
    x <- liftIO $ doesFileExist sliMesh
    if x
      then loadSliMesh sliMesh
      else do
        let fbxMesh = meshPath <> ".fbx"
        y <- liftIO $ doesFileExist fbxMesh
        if y
          then loadFbxMesh fbxMesh
          else loadObjMesh (meshPath <> ".obj")
  _ -> loggingString ("Failed to load (unrecognised mesh extension): " <> meshPath) *> pure []

loadObjMesh :: (MonadIO m, MonadReader env m, HasLogging env, HasStaticScene env, HasTextureLibrary env)
                     => FilePath -> m [MeshSlice]
loadObjMesh meshPath = do
  store <- view (staticScene . sceneMeshes)
  preLoaded <- fetchMeshSlices store meshPath
  case preLoaded of
    Just result -> pure $! snd <$> result
    Nothing -> do
      objE <- liftIO $ parseObjFile meshPath
      case objE of
        Left e -> logging "Failed to parse .obj" *> loggingString e *> pure []
        Right (ObjMesh vs is objMtls mtlLibFile) -> do
          loggingString $ "Loading Mesh: " ++ meshPath
          let mtlName = ((mtlLibFile <> ":") <>)
          lib <- parseMtlFile (replaceFilename meshPath mtlLibFile) >>= mtlLibToMtls mtlName . HM.toList
          let slicer (mtl, start, num) = case HM.lookup mtl lib of
                  Just (ix, op) -> (sliceName, ix, op, sliceIx)
                  Nothing -> (sliceName, defaultMaterial, Opaque, sliceIx)
                where
                  sliceName = mtl <> ('|' : show start)
                  sliceIx = VS.slice start num is
              slices = fmap slicer objMtls
          meshes <- storeMeshes store meshPath vs slices
          pure . fmap snd . catMaybes $ meshes

loadSliMesh :: (MonadIO m, MonadReader env m, HasLogging env, HasStaticScene env, HasTextureLibrary env)
            => FilePath -> m [MeshSlice]
loadSliMesh meshPath = do
  store <- view (staticScene . sceneMeshes)
  preLoaded <- fetchMeshSlices store meshPath
  case preLoaded of
    Just result -> pure $! snd <$> result
    Nothing -> do
      meshM <- fromSliMeshFile meshPath
      case meshM of
        Nothing -> loggingString ("Failed to load " <> meshPath) *> pure []
        Just mesh -> do
          let naming = ((takeFileName meshPath <> ":") <>)
              sliceList = V.toList (_meshSlices mesh)
          mtls <- mtlLibToMtls naming $ M.toList (_meshMaterials mesh)
          let slicer (SliMeshSlice mtl (V2 start num)) = case HM.lookup mtl mtls of
                  Just (ix, op) -> (sliceName, ix, op, sliceIx)
                  Nothing -> (sliceName, defaultMaterial, Opaque, sliceIx)
                where
                  sliceName = mtl <> ('|' : show start)
                  sliceIx = VS.slice start num (_meshIndices mesh)
              slices = fmap slicer sliceList
          meshes <- storeMeshes store meshPath (_meshVertices mesh)  slices
          pure (fmap snd $ catMaybes meshes)

-- NB: fbx path does not handle materials at all yet, but does support vertex colours
loadFbxMesh :: (MonadIO m, MonadReader env m, HasLogging env, HasStaticScene env, HasTextureLibrary env)
            => FilePath -> m [MeshSlice]
loadFbxMesh meshPath = do
  store <- view (staticScene . sceneMeshes)
  preLoaded <- fetchMeshSlices store meshPath
  case preLoaded of
    Just result -> pure $! snd <$> result
    Nothing -> do
      meshM <- liftIO $ fromFBXMeshFile meshPath
      case meshM of
        Left err -> loggingString ("Failed to load " <> meshPath <> "\nwith: " <> err) *> pure []
        Right mesh -> do
          mtlIx <- loadMaterialFallback "*fbx.temp.material" fbxMtl
          meshes <- storeMeshes store meshPath (_fbxMeshVertices mesh) [(_fbxMeshName mesh, mtlIx, Opaque, _fbxMeshIndices mesh)]
          pure . fmap snd . catMaybes $ meshes



mtlLibToMtls :: (MonadIO m, MonadReader env m, HasLogging env, HasStaticScene env, HasTextureLibrary env)
                  => (String -> MaterialKey)
                  -> [(String, MaterialEx MaterialPathsFallback)]
                  -> m (HM.HashMap MaterialKey (MaterialIndex, Opacity))
mtlLibToMtls mtlName l = do
    HM.fromList <$> mapM go l
  where
    go (name, MaterialEx flat transp) = do
      mtlIx <- loadMaterialFallback (mtlName name) flat
      pure (name, (mtlIx, transp))


fbxMtl :: MaterialPathsFallback
fbxMtl = Material
  { _materialBaseColour = ("", V4 0xe0 0xe0 0xe0 0xff)
  , _materialNormal = ("", V4 128 128 255 0xff)
  , _materialAmbientRoughnessMetallicHeight = ("", V4 0xff 0xff 0x00 0x80)
  , _materialSubsurfaceEmissive = ("", V4 0x00 0x00 0x00 0x00)
  }

resizeSceneTargets :: (HasStaticScene env, HasLogging env)
                   => V2 Int -> Quern env ()
resizeSceneTargets sz = do
  loggingString $ "Resizing scene targets to: " ++ show sz
  magnify (staticScene.sceneTargets) $ do
    let sz' = fromIntegral <$> sz
        hz' = (`div` 2) <$> sz'
    mt <- use mainTarget >>= resizeTarget sz'
    bt <- use backfaceTarget >>= resizeTarget hz'
    rt <- use refractionTexture >>= unsafeResizeBufferTexture hz' 6
    gp <- use guiPlane >>= unsafeResizeBufferTexture sz' 1
    ss <- use ssaoTexture >>= unsafeResizeBufferTexture sz' 1
    pt <- use preTarget >>= resizeSharedDepthTarget mt
    mainTarget .= mt
    backfaceTarget .= bt
    preTarget .= pt
    refractionTexture .= rt
    ssaoTexture .= ss
    guiPlane .= gp

clearAllInstances :: (MonadIO m, MonadReader env m, HasStaticScene env) => m ()
clearAllInstances = do
  is <- view (staticScene . sceneInstances)
  ds <- view (staticScene . sceneOpaquePass . passDraws)
  ts <- view (staticScene . sceneTransparentPass . passDraws)
  fs <- view (staticScene . sceneFoliagePass . passDraws)
  storageClear (_instanceStore is)
  indirectStorageClear ds
  indirectStorageClear ts
  indirectStorageClear fs


drawInstance :: (MonadIO m, MonadReader env m, HasLogging env, HasStaticScene env)
             => [(MeshSlice, MaterialIndex, Opacity)]
             -> Instance
             -> m (InstanceIndex, [DrawIndex])
drawInstance slices inst = do
  instStore <- view (staticScene . sceneInstances)
  instIdx <- instanceStore instStore inst
  drawStore <- view (staticScene . sceneOpaquePass . passDraws)
  transpStore <- view (staticScene . sceneTransparentPass . passDraws)
  foliageStore <- view (staticScene . sceneFoliagePass . passDraws)
  addStore <- view (staticScene . sceneAdditivePass . passDraws)
  drawIdxs <- flip traverse slices $ \(slice, mtl, opacity) -> do
    let store = case opacity of
                  Opaque -> drawStore
                  Transparent -> transpStore
                  Masked -> foliageStore
                  Additive -> addStore
    ix <- indirectDrawStore store slice mtl instIdx
    pure ix -- $ DrawIndex opacity ix
  pure (instIdx, drawIdxs)

drawInstanceDflt :: (MonadIO m, MonadReader env m, HasLogging env, HasStaticScene env)
                 => [MeshSlice]
                 -> Instance
                 -> m (InstanceIndex, [DrawIndex])
drawInstanceDflt slices = drawInstance (dflts <$> slices)
  where
    dflts sl = (sl, _meshSliceDefaultMtl sl, _meshSliceDefaultOpacity sl)

newStaticScene :: (MonadIO m, MonadReader env m, HasLogging env)
               => FilePath -> V2 Int -> Multisampling -> m StaticScene
newStaticScene envPath resolution msaa = do
  -- TODO: not... this
  let yolo :: Show a => Either a b -> b
      yolo = either (error.show) id

  logging "newStaticScene"
  glErrorToLog "newStaticScene.enter"
  meshes <- meshStorageNew
  glErrorToLog "newStaticScene.meshStoreNew"
  materials <- materialStorageNew
  glErrorToLog "newStaticScene.materiaStoreNew"
  instances <- instanceStorageNew
  glErrorToLog "newStaticScene.instanceStoreNew"
  draws <- indirectStorageNew Opaque
  transpDraws <- indirectStorageNew Transparent
  foliageDraws <- indirectStorageNew Masked
  addDraws <- indirectStorageNew Additive
  glErrorToLog "newStaticScene.indirectDrawStoreNew"
  cullProg <- yolo <$> loadCullingShader
  glErrorToLog "newStaticScene.loadCullingShader"

  _quadIdx <- storeMeshSlice meshes (MeshKey ("*unit_quad", "*")) defaultMaterial Opaque (makeQuad 1 1) quadIndices

  envCube <- do
    cubeE <- loadAndFilterPanorama envPath Nothing
    case cubeE of
      Left err -> loggingString err *> solidColourTexture2D'RGBA16F (V4 0 0 0 1) Resident
      Right tex -> pure tex
  glErrorToLog "newStaticScene.loadAndFilterPanorama"


  splitSumLUT <- yolo <$> generateSplitSumLUT
  glErrorToLog "newStaticScene.generateSplitSumLUT"

  let shadowFmt = GL_DEPTH_COMPONENT32F
  shadowBuffer <- yolo <$> createTarget (pure 4096) MsNone 0 GL_RGBA8 (Just shadowFmt)
  shadowProg <- yolo <$> loadRenderProgramFromFiles "static_shadows.vert" "" "sun_shadow" Nothing
  renderProg <- yolo <$> loadRenderProgramFromFiles "static_scene.vert" "static_scene.frag" "static_scene" Nothing

  -- eventually...
  let res = fromIntegral <$> resolution -- V2 1920 1080
      sceneFmt = GL_RGBA16F --GL_R11F_G11F_B10F
      halfRes = (`div` 2) <$> res
      sceneDpthFmt = Just GL_DEPTH_COMPONENT32F
  sceneTarget <- either (error.show) id <$> createTarget res msaa 1 sceneFmt sceneDpthFmt
  refractBuffer <- makeBufferTexture halfRes sceneFmt 6
  backfaceTgt <- yolo <$> createTarget halfRes MsNone 1 GL_RGBA16F sceneDpthFmt
  guiPln <- makeBufferTexture res GL_RGBA8 1
  ssaoTx <- makeBufferTextureNonResident res GL_RG16F 1
  preTgt <- yolo <$> depthShareTarget sceneTarget
  let msDefines = if msaa /= MsNone then ["multisample"] else []

  downresProg <- yolo <$> loadKernelFromFile'WithDefines
    "data/shaders/downres.comp" msDefines

  downresTexProg <- if msaa == MsNone
    then pure downresProg
    else yolo <$> loadKernelFromFile "data/shaders/downres.comp"

  presentProg <- yolo <$> loadRenderProgramFromFiles'WithDefines
    "present.vert" "present.frag" msDefines "present" Nothing

  transProg <- yolo <$> loadRenderProgramFromFiles'WithDefines
    "static_scene.vert" "static_scene.frag" ["transparency"] "static_scene_transp" Nothing
  backfaceProg <- yolo <$> loadRenderProgramFromFiles'WithDefines
    "static_scene.vert" "static_scene.frag" ["back_faces"] "static_scene_backface" Nothing

  foliageProg <- yolo <$>
    loadRenderProgramFromFiles'WithDefines
      "static_scene.vert" "static_scene.frag" ["foliage"] "static_scene_foliage" Nothing
  foliageShadowProg <- yolo <$>
    loadRenderProgramFromFiles'WithDefines
      "static_shadows.vert" "static_shadows.frag" ["foliage"] "static_scene_foliage_shadow" Nothing

  -- ssao.comp : uses a sphere of points
  -- hbao.comp : finds horizons in directions
  ssaoProg <- yolo <$>
    loadKernelFromFile'WithDefines "data/shaders/hbao.comp" msDefines

  targets <- liftIO $ newIORef $ SceneTargets
    { _mainTarget = sceneTarget
    , _preTarget = preTgt
    , _backfaceTarget = backfaceTgt
    , _refractionTexture = refractBuffer
    , _ssaoTexture = ssaoTx
    , _guiPlane = guiPln
    }
  pure $ StaticScene
    { _sceneEnvCubemap = envCube
    , _sceneMeshes = meshes
    , _sceneMaterials = materials
    , _sceneInstances = instances
    , _scenePrePassOpaque = ScenePass draws shadowProg Nothing (Just cullProg)
    , _scenePrePassMasked = ScenePass foliageDraws foliageShadowProg Nothing (Just cullProg)
    , _sceneOpaquePass = ScenePass draws renderProg (Just shadowProg) Nothing --(Just cullProg)
    , _sceneFoliagePass = ScenePass foliageDraws foliageProg (Just foliageShadowProg) Nothing
    , _sceneTransparentPass = ScenePass transpDraws transProg (Just shadowProg) Nothing
    , _sceneTransparentBackfacePass = ScenePass transpDraws backfaceProg Nothing Nothing
    , _sceneAdditivePass = ScenePass addDraws renderProg Nothing Nothing
    , _sceneDebug = Nothing
    , _sceneShadowTarget = shadowBuffer
    , _sceneSplitSumLUT = splitSumLUT
    , _sceneTargets = targets
    , _sceneDownresProgram = downresProg
    , _sceneDownresTextureProgram = downresTexProg
    , _sceneSsaoProgram = ssaoProg
    , _scenePresentProgram = presentProg
    }

cullPass :: (HasStaticScene env, HasLogging env) => Camera -> ScenePass -> Quern env ()
cullPass cam pass = do
  scene <- view staticScene
  case _passCullProgram pass of
    Nothing -> pure ()
    Just cp -> do
      useProgram cp
      liftIO $ bindForCompute'MeshDataStorage (_sceneMeshes scene)
      liftIO $ bindForCompute'InstanceStorage (_sceneInstances scene)
      let viewProjection = cameraViewProjection cam
      -- shadow draw cull
      case _passShadowProgram pass of
        Nothing -> pure ()
        Just _ -> do
          shadowDrawCount <- liftIO $ bindForCompute'DrawStorage'Shadow (_passDraws pass)
          let shadowDispatch = fromIntegral $ (shadowDrawCount + 255) `div` 256
              shadowVP = sunlightProj !*! sunlightView cam
          liftIO $ with shadowVP $ glUniformMatrix4fv 0 1 GL_TRUE . castPtr
          glUniform1ui 1 (fromIntegral shadowDrawCount)
          glUniform1ui 2 1
          glDispatchCompute shadowDispatch 1 1

      -- main draw cull
      drawCount <- liftIO $ bindForCompute'DrawStorage  (_passDraws pass)
      -- local size is 256x1x1
      let dispatch = fromIntegral $ (drawCount + 255) `div` 256
      -- set up uniforms
      liftIO $ with viewProjection $ glUniformMatrix4fv 0 1 GL_TRUE . castPtr
      glUniform1ui 1 (fromIntegral drawCount)
      glUniform1ui 2 1 --(if cull then 1 else 0)
      glErrorToLog "cullScene.uniforms"

      -- dispatch and reset state
      glDispatchCompute dispatch 1 1
      glErrorToLog "cullScene.dispatch"

cullScene :: (HasStaticScene env, HasLogging env) => Camera -> Quern env (Maybe GLsync)
cullScene cam = do
  scene <- view staticScene

  -- compute shader writes to indirect buffer, so I guess both are required
  glMemoryBarrier (GL_COMMAND_BARRIER_BIT .|. GL_SHADER_STORAGE_BARRIER_BIT .|. GL_ATOMIC_COUNTER_BARRIER_BIT)
  let sp = _passShadowProgram (_sceneOpaquePass scene)

  cullPass cam (_scenePrePassOpaque scene)
  cullPass cam (_scenePrePassMasked scene)
  cullPass cam (_sceneTransparentPass scene)

  -- clear up
  glUseProgram 0
  forM_ [1..4] $ \i -> glBindBufferBase GL_SHADER_STORAGE_BUFFER i 0
  glErrorToLog "cullScene.clear"

  pure Nothing

-- bunch of binding functions that should be inlined/made less ugly

-- binding for frustum culling
-- the bind points are defined in the frustum culling compute shader
-- they should probably be fetched/calculated rather than hardcoded buuuut I am lazy
bindForCompute'MeshDataStorage :: StaticMeshStorage -> IO ()
bindForCompute'MeshDataStorage store =
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 1 (_storeObject (_meshStoreMeshData store))

bindForCompute'InstanceStorage :: InstanceStorage -> IO ()
bindForCompute'InstanceStorage instances =
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 2 (_storeObject (_instanceStore instances))

bindForCompute'DrawStorage :: StaticIndirectDrawStorage -> IO Int
bindForCompute'DrawStorage store = do
  drawCount <- readIORef (_storeUsage (_packedDraws store))
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 3 (_storeObject (_packedDraws store))
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 4 (_storeObject (_indirectDraws store))
  pure drawCount

bindForCompute'DrawStorage'Shadow :: StaticIndirectDrawStorage -> IO Int
bindForCompute'DrawStorage'Shadow store = do
  drawCount <- readIORef (_storeUsage (_packedDraws store))
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 3 (_storeObject (_packedDraws store))
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 4 (_storeObject (_shadowDraws store))
  pure drawCount


-- binding for drawing
bindMeshStorage :: MonadIO m => StaticMeshStorage -> m ()
bindMeshStorage storage = do
  glBindVertexArray (_meshStoreVAObject storage)

bindIndirectStorage :: MonadIO m => StaticIndirectDrawStorage -> m Int
bindIndirectStorage (StaticIndirectDrawStorage instances _ _ _) = do
  used <- liftIO $ readIORef (_storeUsage instances)
  glBindBuffer GL_DRAW_INDIRECT_BUFFER (_storeObject instances)
  pure used

bindIndirectStorage'Shadows :: MonadIO m => StaticIndirectDrawStorage -> m Int
bindIndirectStorage'Shadows (StaticIndirectDrawStorage _ _ shadows _) = do
  used <- liftIO $ readIORef (_storeUsage shadows)
  glBindBuffer GL_DRAW_INDIRECT_BUFFER (_storeObject shadows)
  pure used

bindMaterialStorage :: MonadIO m => MaterialStorage -> m ()
bindMaterialStorage materials = do
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 0 (_storeObject (_materialStore materials))

bindInstanceStorage :: MonadIO m => InstanceStorage -> m ()
bindInstanceStorage instances = do
  glBindBufferBase GL_SHADER_STORAGE_BUFFER 1 (_storeObject (_instanceStore instances))


sunlightProj :: M44 Float
sunlightProj = orthoLight (size*2) (size*2) depth
  where
    size = 8
    depth = 64

makePos :: Num a => V3 a -> V4 a
makePos (V3 x y z) = V4 x y z 1

-- TODO: snap the position to projected texel centres
sunlightViewSnapped :: Bool -> Camera -> M44 Float
sunlightViewSnapped snap _cam = lookAt (lightAt' + V3 8 4 32) lightAt' (V3 0 0 1)
  where
    --lightAt = _cameraTarget cam * (V3 1 1 0)
    lightAt = V3 8 4 0
    v0 = lookAt (lightAt + V3 8 4 32) lightAt (V3 0 0 1)
    p0 = sunlightProj
    vp0 = p0 !*! v0
    ivp0 = inv44 vp0
    shadowP = makePos lightAt *! vp0
    shadowP' :: V4 Float
    shadowP' = shadowP & _xy %~ fmap (unquantise . quantise)
    lightAt' = if snap then (shadowP' *! ivp0) ^. _xyz * (V3 1 1 0) else lightAt

    quantise :: Float -> Int
    quantise = floor . (* 256)
    unquantise :: Int -> Float
    unquantise = (/ 256) . fromIntegral

sunlightView :: Camera -> M44 Float
sunlightView = sunlightViewSnapped False

sunShadowPass :: (HasStaticScene env, HasLogging env) => ScenePass -> M44 Float -> Maybe (Quern env ()) -> Quern env ()
sunShadowPass pass lightVP beforeDraw = do
  scene <- view staticScene
  glErrorToLog "sunShadowPass.bound"
  case _passShadowProgram pass of
    Nothing -> pure ()
    Just prog -> do
      useProgram prog
      -- bind various buffer objects
      bindMeshStorage (_sceneMeshes scene)
      bindMaterialStorage (_sceneMaterials scene) -- only need this for alpha tested things
      bindInstanceStorage (_sceneInstances scene)
      liftIO $ with lightVP $ glUniformMatrix4fv 0 1 GL_TRUE . castPtr
      case beforeDraw of
        Just action -> action
        Nothing -> pure ()
      toDraw <- bindIndirectStorage'Shadows (_passDraws pass)
      when (toDraw > 0) $
        glMultiDrawElementsIndirect GL_TRIANGLES GL_UNSIGNED_INT nullPtr (fromIntegral toDraw) 0



sunShadowScene :: (HasStaticScene env, HasLogging env) => Float -> Camera -> Quern env (Maybe GLsync)
sunShadowScene renderTime cam = do
    glErrorToLog "sunShadowScene.enter"
    scene <- view staticScene

    --glMemoryBarrier (GL_COMMAND_BARRIER_BIT .|. GL_SHADER_STORAGE_BARRIER_BIT .|. GL_ATOMIC_COUNTER_BARRIER_BIT)

    -- shadows
    let updateShadows = True
        renderShadows = True
        lightView = sunlightView cam
        lightVP = sunlightProj !*! lightView :: M44 Float
        bindTime = liftIO $ with (V4 renderTime 0 0 0) $ glUniform4fv 16 1 . castPtr

    when (updateShadows && renderShadows) $ do
      captureToTarget (_sceneShadowTarget scene) $ \ _res -> do
        glClear GL_DEPTH_BUFFER_BIT
        sunShadowPass (_sceneOpaquePass scene) lightVP Nothing
        sunShadowPass (_sceneTransparentPass scene) lightVP Nothing
        sunShadowPass (_sceneFoliagePass scene) lightVP (Just bindTime)

    pure Nothing


drawPass :: (HasStaticScene env, HasLogging env)
         => Camera -> ScenePass -> T.Text -> Maybe (Quern env ()) -> Bool -> Quern env ()
drawPass cam pass passName beforeDraw bindShadows = do
  glErrorToLog $ "drawPass.start " <> passName
  useProgram (_passProgram pass)
  glErrorToLog $ "drawPass.renderProgram " <> passName
  bindForDraw cam bindShadows
  case beforeDraw of
    Just action -> action
    Nothing -> pure ()
  toDraw <- bindIndirectStorage (_passDraws pass)
  when (toDraw > 0) $ do
    glMultiDrawElementsIndirect GL_TRIANGLES GL_UNSIGNED_INT nullPtr (fromIntegral toDraw) 0
    glErrorToLog $ "drawPass.glMultiDrawElementsIndirect " <> passName


-- | Drawing a scene is simply binding the program and all the buffers,
-- setting some small number of uniforms (the per-frame data),
-- and submitting an indirect draw per pass
drawScene :: (HasStaticScene env, HasLogging env) => Float -> Camera -> Quern env (Maybe GLsync)
drawScene renderTime cam = do
    -- catch errors from other gl use
    glErrorToLog "drawScene.enter"
    scene <- view staticScene

    -- culling
    glMemoryBarrier (GL_COMMAND_BARRIER_BIT .|. GL_SHADER_STORAGE_BARRIER_BIT .|. GL_ATOMIC_COUNTER_BARRIER_BIT)

    SceneTargets sceneTarget backTarget prePassTarget refrTex ssaoTx guiPln <- magnify (staticScene.sceneTargets) get

    let bindTime = liftIO $ with (V4 renderTime 0 0 0) $ glUniform4fv 16 1 . castPtr
        bindSsao = glBindTextureUnit 3 (_textureObject ssaoTx)

    captureToTarget prePassTarget $ \_sz -> do
      glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT
      drawPass cam (_scenePrePassOpaque scene) "pre-opaque" Nothing False
      drawPass cam (_scenePrePassMasked scene) "pre-masked" (Just bindTime) False

    dispatchSsao cam prePassTarget ssaoTx

    -- can do shadowing while ssao kernel is doing things
    _ <- sunShadowScene renderTime cam

    -- ssao needs to be done by now
    glMemoryBarrier (GL_TEXTURE_FETCH_BARRIER_BIT .|. GL_SHADER_IMAGE_ACCESS_BARRIER_BIT)

    -- capture opaques to main scene
    captureToTarget sceneTarget $ \_sz -> do
      -- clear colour, disable depth write because pre-pass fills the buffer
      glClear $ GL_COLOR_BUFFER_BIT
      glDepthMask GL_FALSE
      drawPass cam (_sceneOpaquePass scene) "opaque" (Just bindSsao) True
      drawPass cam (_sceneFoliagePass scene) "foliage" (Just (bindSsao *> bindTime)) True
      glDepthMask GL_TRUE

    downresFromTarget sceneTarget refrTex
    forM_ [0..5] (downresMips refrTex)

    -- this can be done anywhere before transparent rendering
    let renderTranspBackface = True
    when renderTranspBackface $ do
      captureToTarget backTarget $ \_ -> do
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT
        glCullFace GL_FRONT
        drawPass cam (_sceneTransparentBackfacePass scene) "transparent.backfaces" Nothing False
        glCullFace GL_BACK

    -- draw transparents to main scene;
    let refractionLoc = 4
        backfaceLoc = 5
        bindTransparents = do
          glActiveTexture GL_TEXTURE3
          glBindTexture GL_TEXTURE_2D (_textureObject refrTex)
          glUniform1i refractionLoc 3
          glActiveTexture GL_TEXTURE4
          glBindTexture GL_TEXTURE_2D (_textureObject . VS.head . _targetTextures $ backTarget)
          glUniform1i backfaceLoc 4

    captureToTarget sceneTarget $ \_ -> do
      drawPass cam (_sceneTransparentPass scene) "transparent" (Just bindTransparents) True

    -- post process, tonemap, present
    glDepthMask GL_FALSE
    useProgram (_scenePresentProgram scene)
    -- bind main target as texture
    let starget = case sceneTarget^.targetMultisampling of
                    MsNone -> GL_TEXTURE_2D
                    _ -> GL_TEXTURE_2D_MULTISAMPLE
    VS.forM_ (sceneTarget ^. targetTextures) $ \t -> do
      glActiveTexture GL_TEXTURE0
      glBindTexture starget $ _textureObject t --(_targetTextures (_sceneMainTarget scene) VS.! 0)

    glActiveTexture GL_TEXTURE1
    glBindTexture GL_TEXTURE_2D $ _textureObject guiPln

    glUniform1i 0 0
    glUniform1i 1 1

    glDisable GL_CULL_FACE
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
    glErrorToLog "drawScene.glDrawElements(present)"

    mapM_ (\i -> glActiveTexture (GL_TEXTURE0+i) *> glBindTexture GL_TEXTURE_2D 0) [0..4]

    glDepthMask GL_TRUE
    glEnable GL_CULL_FACE
    -- clear bound VAO
    glBindVertexArray 0
    glUseProgram 0
    pure Nothing

-- bind a bunch of uniforms required for drawing opaques or transparents
bindForDraw :: (HasStaticScene env, HasLogging env) => Camera -> Bool -> Quern env ()
bindForDraw cam bindShadows = do
  scene <- view staticScene
  let viewProjection = cameraViewProjection cam
      viewInverse = cameraViewInverse cam
      viewDebugMode = fromIntegral (_cameraDebugMode cam) :: GLint

  -- bind various buffer objects
  bindMeshStorage (_sceneMeshes scene)
  bindMaterialStorage (_sceneMaterials scene)
  bindInstanceStorage (_sceneInstances scene)
  glErrorToLog "bindForDraw.bindStorage"

  let lightView = sunlightView cam
      lightInvView = inv44 lightView
      lightVP = sunlightProj !*! lightView :: M44 Float
      nearFar = V2 (cam^.cameraNear) (fromMaybe 0 (cam^.cameraFar))

  -- set up uniforms for camera matrices, environment, debug mode
  liftIO $ do
    with viewProjection $ glUniformMatrix4fv 0 1 GL_TRUE . castPtr
    with viewInverse    $ glUniformMatrix4fv 1 1 GL_TRUE . castPtr
    with lightVP        $ glUniformMatrix4fv 2 1 GL_TRUE . castPtr
    with lightInvView   $ glUniformMatrix4fv 3 1 GL_TRUE . castPtr
    with nearFar        $ glUniform2fv 17 1 . castPtr
  -- uniforms...
  _ <- glGetError
  if bindShadows
    then do
      let sm = _textureObject <$> _targetDepth (_sceneShadowTarget scene)
      traverse_ (\t -> glBindTextureUnit 1 t *> glBindTextureUnit 9 t) sm
      glErrorToLog "bindForDraw.bindShadow"
      --mapM_ (loggingString . ("bindForDraw: " <>). show) (_targetDepth (_sceneShadowTarget scene))
    else glBindTextureUnit 1 0

  glBindTextureUnit 0 (_textureObject (_sceneEnvCubemap scene))
  glBindTextureUnit 2 (_textureObject (_sceneSplitSumLUT scene))
  glErrorToLog "bindForDraw.bindViewTextures"

  -- TODO: uniform locations refactor/rethink
  let viewDebugLocation = 15
  glUniform1i viewDebugLocation viewDebugMode
  -- debug uniforms are invalid for certain passes, but while developing this is annoying
  -- TODO: throttle logging for per-frame errors to be sane
  _ <- glGetError
  pure ()

downresFromTarget :: (MonadIO m, MonadReader env m, HasStaticScene env) => Target -> Texture -> m ()
downresFromTarget sourceTarget destinationBuffer = do
  prog <- view (staticScene.sceneDownresProgram)
  useProgram prog
  let fmt = destinationBuffer ^. textureFormat
      src = _textureObject $ VS.head (_targetTextures sourceTarget)
      dst = _textureObject destinationBuffer
      srcSize = _targetSize sourceTarget
      dstSize = fromIntegral . (`div` 2) <$> srcSize
      workSize = V2 16 16
      V2 dispX dispY = div <$> (dstSize + (workSize - 1)) <*> workSize
  glBindImageTexture 0 src 0 GL_TRUE 0 GL_READ_ONLY fmt
  glBindImageTexture 1 dst 0 GL_TRUE 0 GL_WRITE_ONLY fmt
  glDispatchCompute dispX dispY 1
  glBindImageTexture 0 0 0 GL_TRUE 0 GL_READ_ONLY  GL_RGBA16F
  glBindImageTexture 1 0 0 GL_TRUE 0 GL_WRITE_ONLY GL_RGBA16F

downresMips :: (MonadIO m, MonadReader env m, HasStaticScene env) => Texture -> GLint -> m ()
downresMips texture srcMip = do
  prog <- view (staticScene.sceneDownresTextureProgram)
  useProgram prog
  let fmt = texture ^. textureFormat
      src = _textureObject texture
      topSize = _textureSize texture
      dstSize = fromIntegral . max 1 . (`div` (2^srcMip)) <$> topSize
      workSize = V2 16 16
      V2 dispX dispY = div <$> (dstSize + (workSize - 1)) <*> workSize
  glBindImageTexture 0 src (srcMip  ) GL_TRUE 0 GL_READ_ONLY  fmt
  glBindImageTexture 1 src (srcMip+1) GL_TRUE 0 GL_WRITE_ONLY fmt
  glDispatchCompute dispX dispY 1
  glBindImageTexture 0 0 0 GL_TRUE 0 GL_READ_ONLY  GL_RGBA16F
  glBindImageTexture 1 0 0 GL_TRUE 0 GL_WRITE_ONLY GL_RGBA16F


updateInstanceSlow :: (MonadIO m, MonadReader env m, HasStaticScene env)
                   => InstanceIndex -> Instance -> m ()
updateInstanceSlow (Inst idx) inst = do
  instStore <- view (staticScene . sceneInstances)
  let target = GL_SHADER_STORAGE_BUFFER
  glBindBuffer target (_storeObject $ _instanceStore instStore)
  let sz = sizeOf inst
      off = fromIntegral (sz * idx)
      sz' = fromIntegral sz
  liftIO $ with inst (glBufferSubData target off sz' . castPtr)

dispatchSsao :: (MonadIO m, MonadReader env m, HasStaticScene env) => Camera -> Target -> Texture -> m ()
dispatchSsao cam depthSrc texOutput = do
  let sz = _targetSize depthSrc
      sz' = fromIntegral <$> sz
      dst = _textureObject texOutput
      dstFmt = _textureFormat texOutput
      src = maybe 0 _textureObject $ _targetDepth depthSrc
      srcTgt = maybe GL_TEXTURE_2D _textureTarget $ _targetDepth depthSrc
      --srcFmt = maybe GL_DEPTH_COMPONENT32F _textureFormat $ _targetDepth depthSrc
      workSize = V2 32 32
      V2 dispX dispY = div <$> (sz' + (workSize - 1)) <*> workSize
      camProj = cameraProjection cam
      invProj = cameraProjectionInverse cam
      nearFar = V2 (cam^.cameraNear) (fromMaybe 0 (cam^.cameraFar))
      c2vy = tan (cam^.cameraFoV * 0.5)
      c2vx = tan (cam^.cameraFoV * cam^.cameraAspectRatio * 0.5)
      clipToView = V2 c2vx c2vy
  prog <- view (staticScene.sceneSsaoProgram)
  useProgram prog
  liftIO $ do
    with camProj $ glUniformMatrix4fv 1 1 GL_TRUE . castPtr
    with invProj $ glUniformMatrix4fv 2 1 GL_TRUE . castPtr
    with nearFar $ glUniform2fv 17 1 . castPtr
    with clipToView $ glUniform2fv 18 1 . castPtr

  glBindTextureUnit 0 src
  let dstUnit = 1
  glBindImageTexture dstUnit dst 0 GL_FALSE 0 GL_WRITE_ONLY dstFmt
  glDispatchCompute dispX dispY 1
  glBindImageTexture dstUnit 0 0 GL_FALSE 0 GL_WRITE_ONLY GL_RGBA16F
  glBindTexture srcTgt 0


-- functions for reloading shaders
reloadPass :: (MonadReader env m, HasLogging env, MonadIO m) => ScenePass -> m ()
reloadPass pass = do
  _ <- reloadProgram (_passProgram pass)
  _ <- traverse reloadProgram (_passShadowProgram pass)
  _ <- traverse reloadProgram (_passCullProgram pass)
  pure ()

reloadScenePrograms :: (MonadReader env m, HasLogging env, MonadIO m) => StaticScene -> m ()
reloadScenePrograms scene = do
  reloadPass (_scenePrePassOpaque scene)
  reloadPass (_scenePrePassMasked scene)
  reloadPass (_sceneOpaquePass scene)
  reloadPass (_sceneFoliagePass scene)
  reloadPass (_sceneTransparentPass scene)
  reloadPass (_sceneTransparentBackfacePass scene)
  reloadPass (_sceneAdditivePass scene)
  _ <- reloadProgram (_sceneDownresProgram scene)
  _ <- reloadProgram (_sceneDownresTextureProgram scene)
  _ <- reloadProgram (_sceneSsaoProgram scene)
  _ <- reloadProgram (_scenePresentProgram scene)
  logging "Reloaded all shaders"
  pure ()
