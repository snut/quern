{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
module Quern.Render.Target
  (
    Target(..)
  , Multisampling(..)
  , captureToTarget
  , createTarget
  , depthShareTarget
  , Texture(..)
  , resizeTarget
  , resizeSharedDepthTarget
  -- * Lenses
  , targetFBO
  , targetSize
  , targetCount
  , targetTextures
  , targetDepth
  , targetMultisampling
  ) where

import Control.Lens (makeLenses)
import Control.Monad (forM, when)
import Control.Monad.IO.Class
import qualified Data.Vector.Storable as VS
import Graphics.GL.Core45
import Graphics.GL.Types

import Foreign
import Linear
import Numeric (showHex)

import Quern.Render.Texture
import Quern.Logging

data Multisampling = MsNone | Ms2x deriving (Eq,Ord,Show,Read,Enum,Bounded)

data Target = Target
  { _targetFBO :: !GLuint
  , _targetSize :: !(V2 GLint)
  , _targetCount :: !GLuint
  , _targetTextures :: !(VS.Vector Texture)
  , _targetDepth :: !(Maybe Texture)
  , _targetMultisampling :: !Multisampling
  } deriving (Eq, Ord, Show)
makeLenses ''Target

resizeTarget :: MonadIO m => V2 GLint -> Target -> m Target
resizeTarget sz tgt = do
  let ms = _targetMultisampling tgt
  txs <- liftIO $ VS.mapM (resizeFBTex ms sz) (_targetTextures tgt)
  dpth <- traverse (resizeFBTex ms sz) (_targetDepth tgt)
  pure $ tgt{ _targetSize = sz, _targetTextures = txs, _targetDepth = dpth }

resizeSharedDepthTarget :: MonadIO m => Target -> Target -> m Target
resizeSharedDepthTarget src tgt
  | matchDepth = pure $ tgt{ _targetSize = _targetSize src, _targetDepth = _targetDepth src }
  | otherwise = error "Mismatched shared depth" *> pure tgt
  where
    matchDepth = fmap _textureObject (_targetDepth src) == fmap _textureObject (_targetDepth tgt)

captureToTarget :: MonadIO m => Target -> (V2 GLint -> m a) -> m a
captureToTarget (Target fbo size@(V2 w h) count _ _ ms) body = preserveFBOState body'
  where
    body' = do
      when (ms /= MsNone) (glEnable GL_MULTISAMPLE)
      bindForCapture w h fbo count
      r <- body size
      when (ms /= MsNone) (glDisable GL_MULTISAMPLE)
      pure r

createTarget :: (HasLogging env, MonadReader env m, MonadIO m) => V2 GLint -> Multisampling -> GLuint -> GLenum -> Maybe GLenum -> m (Either String Target)
createTarget (V2 w h) ms count clrFormat dpthFormat = do
  fbo <- liftIO $ alloca $ \p -> glGenFramebuffers 1 p >> peek p
  glBindFramebuffer GL_FRAMEBUFFER fbo
  texs <- if count <= 0
            then pure []
            else forM [0 .. (count-1)] (createAndAttachColourTex w h ms clrFormat)
  glErrorToLog "createTarget: Colour"
  let flt = if count <= 0 then PcfShadowFilter else PointFilter
  depth <- traverse (createAndAttachFBTex w h ms GL_DEPTH_ATTACHMENT flt) dpthFormat
  glErrorToLog "createTarget: Depth"
  status <- glCheckFramebufferStatus GL_DRAW_FRAMEBUFFER
  glBindFramebuffer GL_FRAMEBUFFER 0
  if status == GL_FRAMEBUFFER_COMPLETE then
    pure . Right $ Target fbo (V2 w h) count (VS.fromList texs) depth ms
  else do
    mapM_ deleteTexture texs
    mapM_ deleteTexture depth
    liftIO $ with fbo $ glDeleteFramebuffers 1
    let details = unwords [show w, show h, 'x':show count, show ms, formatShow clrFormat, maybe "null" formatShow dpthFormat]
    pure . Left $! framebufferError status <> "\n" <> details

depthShareTarget :: (MonadIO m) => Target -> m (Either String Target)
depthShareTarget (Target _ _ _ _ Nothing _) = pure (Left "depthShareTarget with no depth to share")
depthShareTarget (Target _fbo sz _count _clrTexs (Just depth) ms) = do
  fbo <- liftIO $ alloca $ \p -> glGenFramebuffers 1 p >> peek p
  glBindFramebuffer GL_FRAMEBUFFER fbo
  let target = if ms == MsNone then GL_TEXTURE_2D else GL_TEXTURE_2D_MULTISAMPLE
      gltex = _textureObject depth
  glFramebufferTexture2D GL_DRAW_FRAMEBUFFER GL_DEPTH_ATTACHMENT target gltex 0
  status <- glCheckFramebufferStatus GL_DRAW_FRAMEBUFFER
  glBindFramebuffer GL_FRAMEBUFFER 0
  if status == GL_FRAMEBUFFER_COMPLETE then
    pure . Right $ Target fbo sz 0 mempty (Just depth) ms
  else do
    liftIO $ with fbo $ glDeleteFramebuffers 1
    pure . Left $! framebufferError status <> "\n" <> "depthShareTarget failed"


preserveFBOState :: MonadIO m => m a -> m a
preserveFBOState body = do
  -- get current state
  (vx,vy,vw,vh,oldFBO) <- liftIO $ allocaBytes (4 * sizeOf (0 :: GLint)) $ \getPtr -> do
    glGetIntegerv GL_VIEWPORT getPtr
    [vx,vy,vw,vh] <- liftIO $ mapM (peek . plusPtr getPtr . (* sizeOf (0 :: GLint))) [0 .. 3]
    glGetIntegerv GL_DRAW_FRAMEBUFFER_BINDING getPtr
    oldFBO <- peek getPtr
    pure (vx,vy,vw,vh,oldFBO)
  -- run action

  result <- body
  -- restore viewport and fbo
  glBindFramebuffer GL_DRAW_FRAMEBUFFER $ fromIntegral oldFBO
  glViewport vx vy vw vh
  pure result

bindForCapture :: MonadIO m => GLint -> GLint -> GLuint -> GLuint -> m ()
bindForCapture w h fbo count = do
  glViewport 0 0 w h
  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo
  liftIO $ withArray [GL_COLOR_ATTACHMENT0 .. GL_COLOR_ATTACHMENT0 + count - 1] $
    glDrawBuffers (fromIntegral count)


framebufferError :: GLenum -> String
framebufferError e
  | e == GL_FRAMEBUFFER_UNDEFINED = "Framebuffer undefined"
  | e == GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = "Framebuffer attachment incomplete"
  | e == GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = "Framebuffer missing one or more attachments"
  | e == GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = "Framebuffer missing one or more draw buffers"
  | e == GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = "Framebuffer missing one or more read buffers"
  | e == GL_FRAMEBUFFER_UNSUPPORTED = "Framebuffer internal formats not supported"
  | e == GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = "Framebuffer multisample states inconsistent"
  | e == GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = "Framebuffer layered attachments inconsistent"
  | otherwise = "Unknown Framebuffer completeness error: " ++ show e ++ " / 0x" ++ showHex e ""

-- move to elsewhere (texture?)
formatBase :: GLenum -> GLenum
formatBase GL_RGBA8 = GL_RGBA
formatBase GL_RGBA16 = GL_RGBA
formatBase GL_RGBA16F = GL_RGBA
formatBase GL_RGBA32F = GL_RGBA
formatBase GL_R11F_G11F_B10F = GL_RGB
formatBase GL_DEPTH_COMPONENT32F = GL_DEPTH_COMPONENT
formatBase GL_DEPTH_COMPONENT32 = GL_DEPTH_COMPONENT
formatBase GL_DEPTH_COMPONENT24 = GL_DEPTH_COMPONENT
formatBase GL_DEPTH_COMPONENT16 = GL_DEPTH_COMPONENT
formatBase GL_DEPTH24_STENCIL8 = GL_DEPTH_COMPONENT
formatBase _ = error "unknown GL format in formatBase"

formatType :: GLenum -> GLenum
formatType GL_RGBA8 = GL_UNSIGNED_BYTE
formatType GL_RGBA16 = GL_UNSIGNED_SHORT
formatType GL_RGBA32F = GL_FLOAT
formatType GL_RGBA16F = GL_HALF_FLOAT
formatType GL_R11F_G11F_B10F = GL_FLOAT
formatType GL_DEPTH_COMPONENT32F = GL_FLOAT
formatType GL_DEPTH_COMPONENT32 = GL_UNSIGNED_INT
formatType GL_DEPTH_COMPONENT24 = GL_UNSIGNED_INT
formatType GL_DEPTH_COMPONENT16 = GL_UNSIGNED_SHORT
formatType _ = error "unknown GL format in formatType"

formatShow :: GLenum -> String
formatShow GL_RGBA8 = "GL_RGBA8"
formatShow GL_RGBA16 = "GL_RGBA16"
formatShow GL_RGBA32F = "GL_RGBA32F"
formatShow GL_RGBA16F = "GL_RGBA16F"
formatShow GL_R11F_G11F_B10F = "GL_R11F_G11F_B10F"
formatShow GL_DEPTH_COMPONENT32F = "GL_DEPTH_COMPONENT32F"
formatShow GL_DEPTH_COMPONENT32 = "GL_DEPTH_COMPONENT32"
formatShow GL_DEPTH_COMPONENT24 = "GL_DEPTH_COMPONENT24"
formatShow GL_DEPTH_COMPONENT16 = "GL_DEPTH_COMPONENT16"
formatShow x = showHex x ""

resizeFBTex :: MonadIO m => Multisampling -> V2 GLint -> Texture ->  m Texture
resizeFBTex ms sz@(V2 w h) tex  = do
  let target = if ms == MsNone then GL_TEXTURE_2D else GL_TEXTURE_2D_MULTISAMPLE
      fmt = _textureFormat tex
  glBindTexture target (_textureObject tex)
  case ms of
    MsNone -> glTexImage2D target 0 (fromIntegral fmt) w h 0 (formatBase fmt) (formatType fmt) nullPtr
    Ms2x -> glTexImage2DMultisample target 2 fmt w h GL_FALSE
  pure $ tex{ _textureSize = sz }

data TargetFiltering
  = PointFilter
  | LinearFilter
  | PcfShadowFilter
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- note dirty thing: if filtering is enabled on a depth target,
-- set it up for PCF via depth comparison mode
createAndAttachFBTex :: (HasLogging env, MonadReader env m, MonadIO m) => GLint -> GLint -> Multisampling -> GLenum -> TargetFiltering -> GLenum -> m Texture
createAndAttachFBTex w h ms attachment filtering fmt = do
    gltex <- liftIO $ alloca $ \ptr -> glGenTextures 1 ptr *> peek ptr
    glErrorToLog "createAndAttachFBTex: Generate"
    glBindTexture target gltex
    glErrorToLog "createAndAttachFBTex: Bind"
    when (ms == MsNone) $ do
      glTexParameteri target GL_TEXTURE_MAG_FILTER fmode
      glTexParameteri target GL_TEXTURE_MIN_FILTER fmode
      glTexParameteri target GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
      glTexParameteri target GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
    glTexParameteri target GL_TEXTURE_MAX_LEVEL 0
    glErrorToLog "createAndAttachFBTex: Parameters"

    -- for shadows and such, probably better to just have the border max z
    when (attachment == GL_DEPTH_ATTACHMENT && ms == MsNone) $ do
      glTexParameteri target GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
      glTexParameteri target GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
      liftIO $ with opaqueWhite $ glTexParameterfv target GL_TEXTURE_BORDER_COLOR . castPtr
      glErrorToLog "createAndAttachFBTex: Depth 1"

    when (attachment == GL_DEPTH_ATTACHMENT && filtering == PcfShadowFilter && ms == MsNone) $ do
      glTexParameteri target GL_TEXTURE_COMPARE_MODE GL_COMPARE_REF_TO_TEXTURE
      glTexParameteri target GL_TEXTURE_COMPARE_FUNC GL_GREATER -- revZ: default GL_LESS
      glErrorToLog "createAndAttachFBTex: Depth 2"

    let baseFmt = formatBase fmt
        compType = formatType fmt
    case ms of
      MsNone -> glTexImage2D target 0 (fromIntegral fmt) w h 0 baseFmt compType nullPtr
      Ms2x -> glTexImage2DMultisample target 2 fmt w h GL_FALSE
    glFramebufferTexture2D GL_DRAW_FRAMEBUFFER attachment target gltex 0
    glErrorToLog "createAndAttachFBTex: Attach"
    pure $ Texture target gltex 0 fmt (V2 w h)
  where
    opaqueWhite = 1 :: V4 Float
    target = case ms of
              MsNone -> GL_TEXTURE_2D
              Ms2x -> GL_TEXTURE_2D_MULTISAMPLE
    fmode = case filtering of
              PointFilter -> GL_NEAREST
              LinearFilter -> GL_LINEAR
              PcfShadowFilter -> GL_LINEAR

createAndAttachColourTex ::(HasLogging env, MonadReader env m, MonadIO m) => GLint -> GLint -> Multisampling -> GLenum -> GLuint -> m Texture
createAndAttachColourTex w h ms fmt i = createAndAttachFBTex w h ms attachment LinearFilter fmt
  where
    attachment = i + GL_COLOR_ATTACHMENT0
