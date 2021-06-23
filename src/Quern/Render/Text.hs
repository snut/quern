{-| Handles rendering a bunch of scree-space quads, which can be text characters or shapes -}

module Quern.Render.Text
  ( RenderText(..)
  , TextStyle(..)
  , defaultTextStyle
  , newRenderText
  , setRenderTextRes
  , addString
  , addStringStyled
  , GuiBox(..)
  , addGuiBox
  , addGuiBoxes
  , clearAllText
  , drawText
  , textSize
  ) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import Data.IORef
import Foreign
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Quern.Font.BMFont
import Quern.Font.Types
import Quern.Render.Internal.StorageBuffer
import Quern.Render.Shader
import Quern.Render.Texture
import System.FilePath ((</>), takeDirectory)

import Quern.Logging
import Quern.Render.GL

import Quern.Physics.AxisAlignedBox (AABox, size)

data RenderText = RenderText
  { _renderTextFont :: !Font
  , _renderTextTexture :: !Texture
  , _renderTextProgram :: !Program
  , _renderTextVBuffer :: !GLuint
  , _renderTextIBuffer :: !GLuint
  , _renderTextVAO :: !GLuint
  , _renderTextUsage :: !(IORef Int)
  , _renderTextRes :: !(IORef (V2 Int))
  }


textSize :: RenderText -> Float -> String -> V2 Int
textSize rt scl str = size (_textQuadsBounds q)
  where
    q  = textQuads (_renderTextFont rt) 0 stl str
    stl = defaultTextStyle{_textStyleSize = scl}

generateQuadIndices :: Int -> VS.Vector (V2 (V3 Word16))
generateQuadIndices n = VS.generate n $ go . fromIntegral . (* 4)
  where
    go j = V2 (V3 j (j+1) (j+2)) (V3 (j+1) (j+3) (j+2))

fontVertexFormatBound :: MonadIO m => m ()
fontVertexFormatBound  = do
  flip mapM_ [0..4] $ \i -> do
    glEnableVertexAttribArray  i
    glVertexAttribBinding  i 0
  glVertexAttribFormat 0 2 GL_FLOAT GL_FALSE 0
  glVertexAttribFormat 1 2 GL_FLOAT GL_FALSE 8
  glVertexAttribFormat 2 4 GL_UNSIGNED_BYTE GL_TRUE 16
  glVertexAttribFormat 3 4 GL_UNSIGNED_BYTE GL_TRUE 20
  glVertexAttribIFormat 4 4 GL_UNSIGNED_INT 24

renderTextUsageOffset :: MonadIO m => RenderText -> m GLintptr
renderTextUsageOffset = fmap (fromIntegral . (*textQuadSize)) . liftIO . readIORef . _renderTextUsage

renderTextRes :: MonadIO m => RenderText -> m (V2 Int)
renderTextRes = liftIO . readIORef . _renderTextRes

setRenderTextRes :: MonadIO m => RenderText -> V2 Int -> m ()
setRenderTextRes rt = liftIO . writeIORef (_renderTextRes rt)

newRenderText :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> V2 Int -> m RenderText
newRenderText path res = do
  let explodeLefts :: Show a => Either a b -> b
      explodeLefts = either (error.show) id

  font <- explodeLefts <$> liftIO (parseBMFontFile path)
  let page = case IM.lookup 0 (_fontPages font) of
                Just p -> p
                Nothing -> error "Missing font page zero!"
  texture <- explodeLefts <$> fileTexture2D (takeDirectory path </> BS.unpack (_pageFile page)) Resident

  let maxCharQuads = 16384
      vtxStride = fromIntegral $ sizeOf (undefined :: TextVertex)

  vb <- newBufferObject GL_ARRAY_BUFFER (maxCharQuads * textQuadSize) (GL_DYNAMIC_STORAGE_BIT .|. GL_MAP_WRITE_BIT)
  ib <- liftIO $ VS.unsafeWith (generateQuadIndices maxCharQuads) $ \ptr ->
          newBufferObjectFromPtr GL_ELEMENT_ARRAY_BUFFER (maxCharQuads * 6 * 2) 0 (castPtr ptr)

  vao <- allocateObject (glGenVertexArrays 1)
  glBindVertexArray vao
  fontVertexFormatBound
  glBindVertexBuffer 0 vb 0 vtxStride
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ib
  glBindVertexArray 0

  prog <- explodeLefts <$>loadRenderProgramFromFiles "text.vert" "text.frag" "Font" Nothing

  usage <- liftIO $ newIORef 0
  resRef <- liftIO $ newIORef res
  pure $ RenderText font texture prog vb ib vao usage resRef

clearAllText :: MonadIO m => RenderText -> m ()
clearAllText rt = liftIO $ writeIORef (_renderTextUsage rt) 0

addVerts :: MonadIO m => RenderText -> TextQuads -> m ()
addVerts rt (TextQuads vs _) = do
  let vbSize = VS.length vs * textQuadSize
      target = GL_ARRAY_BUFFER
      vb = _renderTextVBuffer rt
  offset <- renderTextUsageOffset rt
  glBindBuffer target vb
  dstPtr <- glMapBufferRange target offset (fromIntegral vbSize) (GL_MAP_WRITE_BIT .|. GL_MAP_INVALIDATE_RANGE_BIT)
  liftIO $ VS.unsafeWith vs $ \srcPtr -> copyBytes dstPtr (castPtr srcPtr) vbSize
  _ <- glUnmapBuffer target
  liftIO $ modifyIORef' (_renderTextUsage rt) (+ VS.length vs)
  pure ()

addGuiBox :: (MonadIO m) => RenderText -> AABox V2 Int -> Int -> Int -> V4 Word8 -> V4 Word8 -> m ()
addGuiBox rt box rnd sft fg bg = addVerts rt $ guiRect box rnd sft fg bg

data GuiBox = GuiBox
  { _guiBoxSize :: !(AABox V2 Int)
  , _guiBoxRound :: !Int
  , _guiBoxSoft :: !Int
  , _guiBoxForeground :: !(V4 Word8)
  , _guiBoxBackground :: !(V4 Word8)
  } deriving (Eq, Ord, Show, Read)

addGuiBoxes :: (MonadIO m, Foldable f) => RenderText -> f GuiBox -> m ()
addGuiBoxes rt bs = addVerts rt (vs)
  where
    vs = foldMap (go) bs
    go (GuiBox sz rnd sft fg bg) = guiRect sz rnd sft fg bg

addString :: (MonadIO m) => RenderText -> V2 Int -> Float -> String -> m ()
addString rt pos scl = addStringStyled rt pos defaultTextStyle{_textStyleSize = scl}

addStringStyled :: (MonadIO m) => RenderText -> V2 Int -> TextStyle -> String -> m ()
addStringStyled rt pos stl str = do
  addVerts rt $ textQuads (_renderTextFont rt) pos stl str --res


drawText :: (MonadIO m) => RenderText -> m ()
drawText rt = do
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  count <- liftIO $ readIORef (_renderTextUsage rt)
  V2 resX resY <- renderTextRes rt
  glBindVertexArray (_renderTextVAO rt)
  useProgram (_renderTextProgram rt)
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D (_textureObject (_renderTextTexture rt))
  glUniform1i 0 0
  glUniform2f 1 (fromIntegral resX) (fromIntegral resY)
  glDrawElements GL_TRIANGLES (fromIntegral count * 6) GL_UNSIGNED_SHORT nullPtr
  glDisable GL_BLEND
