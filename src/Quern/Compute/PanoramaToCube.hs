{-# Language OverloadedStrings #-}

module Quern.Compute.PanoramaToCube
  ( loadAndFilterPanorama
  , makePrefilteredCubeMap
  ) where


import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Foldable (forM_)
import qualified Data.Text as T
import Graphics.GL.Core45
import Graphics.GL.Types
import Foreign
import Linear

import Quern.Logging
import Quern.Render.GL
import Quern.Render.Shader
import Quern.Render.Texture


-- | Load a HDR environment from a panoramic image and convert it to
-- a cubemap with preconvolved mipmaps for different roughness values.
loadAndFilterPanorama :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> Maybe Texture -> m (Either String Texture)
loadAndFilterPanorama path old = runExceptT $ do
    env <- ExceptT $ fileTexture2D path NonResident
    envCube <- ExceptT $ makePrefilteredCubeMap env
    liftIO $ with (_textureObject env) $ glDeleteTextures 1
    mapM_ (liftIO . deleteTexture) old
    pure envCube

-- | Convert a 2D panorama texture to a cubemap, with the mips prefiltered
-- for different roughness values.
-- The resulting cubemap is 256x256 at the top mip, and workGroupSize at the final level.
--
-- NB: currently loads and deletes the compute program every invocation,
-- not very sensible for repeated use.
makePrefilteredCubeMap :: (HasLogging env, MonadReader env m, MonadIO m) => Texture -> m (Either String Texture)
makePrefilteredCubeMap panaEnv = do
  let texSize = 256
  glBindTexture GL_TEXTURE_2D 0
  glBindTexture GL_TEXTURE_CUBE_MAP 0
  progE <- loadKernelFromFile "./data/shaders/panorama_to_cube.comp"
  case progE of
    Left e -> pure . Left $ e
    Right p -> do
      groupDim@(V3 groupX groupY groupZ) <- liftIO $ allocaArray 3 $ \ptr -> do
        glGetProgramiv (_programObject p) GL_COMPUTE_WORK_GROUP_SIZE ptr
        traverse (peekElemOff ptr) (V3 0 1 2)
      when (groupX /= groupY || groupZ > 1) $
        loggingString $ "Prefiltered cubemap group size mismatch: " ++ show groupDim
      let groupSize = groupX
      glMemoryBarrier GL_TEXTURE_UPDATE_BARRIER_BIT
      glErrorToLog "makePrefilteredCubeMap.init"
      glBindImageTexture 0 (_textureObject panaEnv) 0 GL_TRUE 0 GL_READ_ONLY GL_RGBA16F
      glErrorToLog "makePrefilteredCubeMap.bindPanorama"
      glUseProgram (_programObject p)
      glUniform1ui 0 (fromIntegral texSize)
      glErrorToLog "makePrefilteredCubeMap.useProgram"
      --let mipMax = round $ logBase (2::Double) (fromIntegral texSize) - logBase 2 (fromIntegral groupSize)
      let mipMax = round $ logBase (2 :: Double) (fromIntegral texSize)
      cubeEnv <- makeCubemap texSize mipMax
      forM_ [0 .. mipMax] $ \level -> do
        let mipSize = texSize `div` (2^level)
            numGroups = max 1 . fromIntegral $ mipSize `div` groupSize
        glBindImageTexture 1 (_textureObject cubeEnv) level GL_TRUE 0 GL_WRITE_ONLY GL_RGBA16F
        glDispatchCompute numGroups numGroups 6
        glErrorToLog $ T.pack ("makePrefilteredCubeMap.dispatch.mip[" ++ show level ++ "]")
      glMemoryBarrier GL_TEXTURE_UPDATE_BARRIER_BIT
      glUseProgram 0
      glDeleteProgram (_programObject p)
      glBindImageTexture 0 0 0 GL_TRUE 0 GL_READ_ONLY  GL_RGBA16F
      glBindImageTexture 1 0 0 GL_TRUE 0 GL_WRITE_ONLY GL_RGBA16F
      glErrorToLog "makePrefilteredCubeMap.cleanup"
      pure $ Right cubeEnv


makeCubemap :: MonadIO m => GLsizei -> GLsizei -> m Texture
makeCubemap sz mipMax = do
  tex <- allocateObject (glGenTextures 1)
  let target = GL_TEXTURE_CUBE_MAP
  glBindTexture target tex
  glTexStorage2D target (mipMax + 1) GL_RGBA16F sz sz
  glTexParameteri target GL_TEXTURE_BASE_LEVEL 0
  glTexParameteri target GL_TEXTURE_MAX_LEVEL mipMax

  glTexParameteri target GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
  glTexParameteri target GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri target GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri target GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
  glTexParameteri target GL_TEXTURE_CUBE_MAP_SEAMLESS 1

  glBindTexture target 0
  pure (Texture target tex 0 GL_RGBA16F (pure sz))
