{-# Language OverloadedStrings #-}

module Quern.Compute.PrecomputeBRDF
  ( generateSplitSumLUT
  ) where

import Control.Monad.IO.Class
import Foreign
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Quern.Logging
import Quern.Render.GL
import Quern.Render.Shader
import Quern.Render.Texture

generateSplitSumLUT :: (HasLogging env, MonadReader env m, MonadIO m) => m (Either String Texture)
generateSplitSumLUT = do
  progE <- loadKernelFromFile "data/shaders/split_sum_brdf.comp"
  case progE of
    Left e -> pure (Left e)
    Right p -> do
      let size = 512
      lut <- makeLutTexture 512

      groupDim@(V3 groupX groupY _groupZ) <- liftIO $ allocaArray 3 $ \ptr -> do
        glGetProgramiv (_programObject p) GL_COMPUTE_WORK_GROUP_SIZE ptr
        traverse (peekElemOff ptr) (V3 0 1 2)

      glBindImageTexture 0 (_textureObject lut) 0 GL_TRUE 0 GL_WRITE_ONLY GL_RG16
      glErrorToLog "generateLUT.bindLUT"
      glUseProgram (_programObject p)
      let dispX = max 1 . fromIntegral $ size `div` groupX
          dispY = max 1 . fromIntegral $ size `div` groupY
      loggingString $ "generateLUT: " ++ show groupDim ++ " x " ++ show (V3 dispX dispY 1)
      glDispatchCompute dispX dispY 1
      glErrorToLog "generateLUT.dispatch"
      glMemoryBarrier GL_TEXTURE_UPDATE_BARRIER_BIT
      glUseProgram 0
      glDeleteProgram (_programObject p)
      glBindImageTexture 0 0 0 GL_TRUE 0 GL_READ_ONLY  GL_RG16
      glErrorToLog "generateLUT.cleanup"

      pure $ Right lut

makeLutTexture :: MonadIO m => GLsizei -> m Texture
makeLutTexture sz = do
  tex <- allocateObject (glGenTextures 1)
  let target = GL_TEXTURE_2D
  glBindTexture target tex
  glTexStorage2D target 1 GL_RG16 sz sz
  glTexParameteri target GL_TEXTURE_BASE_LEVEL 0
  glTexParameteri target GL_TEXTURE_MAX_LEVEL 0
  glTexParameteri target GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri target GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri target GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri target GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
  glBindTexture target 0
  pure (Texture target tex 0 GL_RG16 (pure sz))
