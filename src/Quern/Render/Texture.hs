{-# Language TemplateHaskell #-}

module Quern.Render.Texture
  ( -- * Types
    Texture(..)
  , TextureResidency(..)
  , nullTexture
  , textureObject
  , textureTarget
  , textureHandle
  , textureFormat
  , textureSize
  -- * 2D textures
  , solidColourTexture2D
  , solidColourTexture2D'RGBA16F
  , fileTexture2D
  , fileTexture2D'WithDefault
  , deleteTexture
  , debugTexture2D
  -- * Low level helpers
  , makeBufferTexture
  , makeBufferTextureNonResident
  , unsafeResizeBufferTexture
  -- * Library
  , newTextureLibrary
  , pushTextureLibrary
  , popTextureLibrary
  , textureFromAction
  , textureFromFile
  , textureFromFile'WithDefaultColour
  , textureToLibrary
  , TextureLibrary(..)
  , HasTextureLibrary(..)
  ) where

import Codec.Picture
import Codec.Picture.Types (convertImage)
import Control.Lens (makeLenses, Lens')
import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign
import Graphics.GL.Core45
import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic
import Graphics.GL.Types
import Linear
import Numeric.Half

import Quern.Render.GL
import Quern.Logging

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (traverse_)



data Texture = Texture
  { _textureTarget :: !GLenum
  , _textureObject :: !GLuint
  , _textureHandle :: !GLuint64
  , _textureFormat :: !GLenum
  , _textureSize   :: !(V2 GLsizei)
  } deriving (Eq, Ord, Show)

makeLenses ''Texture


data TextureResidency
  = NonResident
  | Resident
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- Keep a stack of texture libraries
newtype TextureLibrary = TextureLibrary
  { _textureLibrary :: IORef [HashMap Text Texture]
  }

class HasTextureLibrary a where
  textureLibrary :: Lens' a TextureLibrary

{-
None of this helps prevent the render thread from attempting to use
a texture that has been destroyed, particularly dangerous when using
resident texture handles for bindless access

One option might be to use a stack of resource libraries with
promoted depth in the types, and have resources only able to depend
on resoures at the same or lower depth, then release entire libraries at once
-}
newTextureLibrary :: MonadIO m => m TextureLibrary
newTextureLibrary = liftIO $ TextureLibrary <$> newIORef []

pushTextureLibrary :: MonadIO m => TextureLibrary -> m ()
pushTextureLibrary (TextureLibrary libRef) = liftIO $ do
  libs <- readIORef libRef
  case libs of
    (lib:rest)
      | HM.null lib -> pure ()
      | otherwise -> case rest of
        (lib':_) | lib' == lib -> pure () -- don't continue pushing empty deltas
        _ -> modifyIORef' libRef (lib:)
    [] -> modifyIORef' libRef (mempty:)

popTextureLibrary :: MonadIO m => TextureLibrary -> m ()
popTextureLibrary (TextureLibrary libRef) = liftIO $ do
  libs <- readIORef libRef
  let (toFree, tl) = case libs of
        [] -> ([], [])
        [lib] -> (HM.elems lib, [])
        (lib1:lib0:libTl) -> (HM.elems (HM.difference lib1 lib0), (lib0:libTl))
  traverse_ deleteTexture toFree -- $ HM.lookup name lib
  writeIORef libRef tl

-- consider keeping a parallel map of texture object -> name/name set
-- to avoid multiply adding a texture and deleting it at the wrong stack level
textureToLibrary :: MonadIO m => TextureLibrary -> Text -> Texture -> m ()
textureToLibrary (TextureLibrary libRef) name tex = liftIO $ do
  libs <- readIORef libRef
  case libs of
    (lib:rest) -> writeIORef libRef (HM.insert name tex lib : rest)
    [] -> writeIORef libRef [HM.singleton name tex]

textureFromAction :: MonadIO m => TextureLibrary -> Text -> IO Texture -> m Texture
textureFromAction (TextureLibrary libRef) name gen = liftIO $ do
  libs <- readIORef libRef
  case libs of
    [] -> do
      newT <- gen
      writeIORef libRef [HM.singleton name newT]
      pure newT
    (lib:rest) ->
      case HM.lookup name lib of
        Just oldT -> pure oldT
        Nothing -> do
          newT <- gen
          writeIORef libRef (HM.insert name newT lib : rest)
          pure newT

textureFromFile :: MonadIO m => TextureLibrary -> FilePath -> TextureResidency -> m Texture
textureFromFile lib path residency = textureFromAction lib path' gen
  where
    path' = T.pack path
    gen = do
      newE <- fileTexture2D path residency
      case newE of
        Right t -> pure t
        Left _ -> debugTexture2D residency

textureFromFile'WithDefaultColour :: MonadIO m => TextureLibrary -> FilePath -> TextureResidency -> V4 Word8 -> m Texture
textureFromFile'WithDefaultColour lib [] residency clr = textureFromAction lib (T.pack (show clr)) (solidColourTexture2D clr residency)
textureFromFile'WithDefaultColour lib path residency clr = textureFromAction lib path' gen
  where
    path' = T.pack path
    gen = do
      newE <- fileTexture2D path residency
      case newE of
        Right t -> pure t
        Left _ -> solidColourTexture2D clr residency


nullTexture :: Texture
nullTexture = Texture 0 0 0 GL_RGBA8 0

instance Storable Texture where
  sizeOf _ =
    sizeOf (0 :: GLenum) * 2 +
    sizeOf (0 :: GLuint) +
    sizeOf (0 :: GLuint64) +
    sizeOf (0 :: GLsizei) * 2
  alignment _ = 1
  peek ptr = do
    let szE = sizeOf (0 :: GLenum)
        szUI = sizeOf (0 :: GLuint)
        szUI64 = sizeOf (0 :: GLuint64)
    tg <- peek (ptr `plusPtr` 0)
    nm <- peek (ptr `plusPtr` szE)
    hd <- peek (ptr `plusPtr` (szE + szUI))
    fm <- peek (ptr `plusPtr` (szE + szUI + szUI64))
    sz <- peek (ptr `plusPtr` (szE + szUI + szUI64 + szE))
    pure $ Texture tg nm hd fm sz
  poke ptr (Texture tg nm hd fm sz) = do
    let szE = sizeOf (0 :: GLenum)
        szUI = sizeOf (0 :: GLuint)
        szUI64 = sizeOf (0 :: GLuint64)
    poke (ptr `plusPtr` 0) tg
    poke (ptr `plusPtr` szE) nm
    poke (ptr `plusPtr` (szE + szUI)) hd
    poke (ptr `plusPtr` (szE + szUI + szUI64)) fm
    poke (ptr `plusPtr` (szE + szUI + szUI64 + szE)) sz

deleteTexture :: MonadIO m => Texture -> m ()
deleteTexture (Texture _ tex hdl _ _) = do
  when (hdl /= 0) (glMakeTextureHandleNonResidentARB hdl)
  liftIO $ with tex $ glDeleteTextures 1

debugPixels :: VS.Vector (V4 Word8)
debugPixels = VS.fromList [V4 r g b a |  a <- [0,0xff], b <- [0,0xff], g <- [0,0xff], r <- [0,0xff]]

texResident :: MonadIO m => GLuint -> TextureResidency -> m GLuint64
texResident _  NonResident = pure 0
texResident tex Resident = do
    hdl <- glGetTextureHandleARB tex
    glMakeTextureHandleResidentARB hdl
    pure hdl

debugTexture2D :: MonadIO m => TextureResidency -> m Texture
debugTexture2D resident = do
  tex <- liftIO $ alloca $ \ptr -> glGenTextures 1 ptr *> peek ptr
  glBindTexture GL_TEXTURE_2D tex
  liftIO $ VS.unsafeWith debugPixels $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 4 4 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  hdl <- texResident tex resident
  glBindTexture GL_TEXTURE_2D 0
  pure $! Texture
    { _textureTarget = GL_TEXTURE_2D
    , _textureObject = tex
    , _textureHandle = hdl
    , _textureFormat = GL_RGBA8
    , _textureSize = 4 }


-- | Create a 4x4 texture with a solid colour and mipmaps, and make it resident
solidColourTexture2D :: MonadIO m => V4 Word8 -> TextureResidency -> m Texture
solidColourTexture2D clr resident = do
    tex <- allocateObject (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D tex
    liftIO $ do
      VS.unsafeWith pixels $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 mip0 mip0 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr
      VS.unsafeWith pixels $ glTexImage2D GL_TEXTURE_2D 1 GL_RGBA8 mip1 mip1 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr
      VS.unsafeWith pixels $ glTexImage2D GL_TEXTURE_2D 2 GL_RGBA8 mip2 mip2 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    hdl <- texResident tex resident
    glBindTexture GL_TEXTURE_2D 0
    pure $! Texture
      { _textureTarget = GL_TEXTURE_2D
      , _textureObject = tex
      , _textureHandle = hdl
      , _textureFormat = GL_RGBA8
      , _textureSize = pure mip0 }
  where
    mip0 = 4
    mip1 = mip0 `div` 2
    mip2 = mip1 `div` 2
    pixels = VS.replicate (fromIntegral (mip0*mip0)) clr

solidColourTexture2D'RGBA16F :: MonadIO m => V4 Half -> TextureResidency -> m Texture
solidColourTexture2D'RGBA16F clr resident = do
    tex <- allocateObject (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D tex
    liftIO $ do
      VS.unsafeWith pixels0 $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA16F mip0 mip0 0 GL_RGBA GL_HALF_FLOAT . castPtr
      VS.unsafeWith pixels1 $ glTexImage2D GL_TEXTURE_2D 1 GL_RGBA16F mip1 mip1 0 GL_RGBA GL_HALF_FLOAT . castPtr
      VS.unsafeWith pixels2 $ glTexImage2D GL_TEXTURE_2D 2 GL_RGBA16F mip2 mip2 0 GL_RGBA GL_HALF_FLOAT . castPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    hdl <- texResident tex resident
    glBindTexture GL_TEXTURE_2D 0
    pure $! Texture
      { _textureTarget = GL_TEXTURE_2D
      , _textureObject = tex
      , _textureHandle = hdl
      , _textureFormat = GL_RGBA16F
      , _textureSize = pure mip0 }
  where
    mip0 = 4
    mip1 = mip0 `div` 2
    mip2 = mip1 `div` 2
    pixels0 = VS.replicate (4*4) clr
    pixels1 = VS.replicate (2*2) clr
    pixels2 = VS.replicate (1*1) clr

fileTexture2D'WithDefault :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> TextureResidency -> Texture -> m Texture
fileTexture2D'WithDefault path resident fallback = do
  texE <- fileTexture2D path resident
  case texE of
    Right t -> pure t
    Left errMsg -> do
      loggingString ("Failed to load texture: " ++ path)
      loggingString ("Reason: " ++ errMsg)
      pure fallback

-- | Uses JuicyPixels to generate a texture from an image stored in one of a number
-- of common file formats (png, jpg, hdr, tga, etc.) - notable no compressed formats.
-- Mipmaps are generated automatically for the texture, and it is made resident if requested.
fileTexture2D :: MonadIO m => FilePath -> TextureResidency -> m (Either String Texture)
fileTexture2D [] _ = pure $ Left "null path"
fileTexture2D path makeResident  = liftIO $ do
  imgErr <- readImage path
  case imgErr of
    Left errMsg -> pure (Left errMsg)
    Right dyn -> case dyn of
      ImageRGBF  (Image w h pixels) -> Right <$> tex2D makeResident w h (hdr pixels) GL_RGBA16F GL_RGBA  GL_HALF_FLOAT
      ImageYF    (Image w h pixels) -> Right <$> tex2D makeResident w h pixels GL_R32F   GL_RED  GL_FLOAT
      ImageRGBA8 (Image w h pixels) -> Right <$> tex2D makeResident w h pixels GL_RGBA8  GL_RGBA GL_UNSIGNED_BYTE
      ImageRGB8  (Image w h pixels) -> Right <$> tex2D makeResident w h pixels GL_RGB8   GL_RGB  GL_UNSIGNED_BYTE
      ImageY8    (Image w h pixels) -> Right <$> tex2D makeResident w h pixels GL_R8     GL_RED  GL_UNSIGNED_BYTE
      ImageYCbCr8 img -> let Image w h pixels = convertImage img :: Image PixelRGB8
                         in Right <$> tex2D makeResident w h pixels GL_RGBA8  GL_RGBA GL_UNSIGNED_BYTE
      _ -> pure . Left $ "Unrecognised texture format: " ++ path

hdr :: VS.Vector Float -> VS.Vector (V4 Half)
hdr fs = VS.generate (VS.length fs `div` 3) go
  where
    go j = V4 r g b 1
      where
        i = j*3
        r = toHalf $ fs VS.! i
        g = toHalf $ fs VS.! (i+1)
        b = toHalf $ fs VS.! (i+2)

-- helper
tex2D :: (MonadIO m, Storable p) => TextureResidency -> Int -> Int -> VS.Vector p -> GLint -> GLenum -> GLenum -> m Texture
tex2D resident w h pixels sizedFmt {-GL_RGBA8-} pixelFmt {-GL_RGB-} componentFmt {-GL_UNSIGNED_BYTE-} = do
    tex <- allocateObject (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D tex
    liftIO $ VS.unsafeWith pixels $ glTexImage2D GL_TEXTURE_2D 0 sizedFmt w' h' 0 pixelFmt componentFmt . castPtr
    glGenerateMipmap GL_TEXTURE_2D
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    when gl_EXT_texture_filter_anisotropic $ do
      aniso <- allocateObject (glGetFloatv GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT)
      glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT ( max 1 . min 16 $ aniso)
    hdl <- texResident tex resident
    glBindTexture GL_TEXTURE_2D 0
    pure $! Texture
      { _textureTarget = GL_TEXTURE_2D
      , _textureObject = tex
      , _textureHandle = hdl
      , _textureFormat = fromIntegral sizedFmt
      , _textureSize = V2 w' h' }
  where
    w' = fromIntegral w
    h' = fromIntegral h

maxMip :: V2 GLsizei -> GLint
maxMip (V2 w h) = ceiling (max mw mh)
  where
    mw = logBase 2 (fromIntegral w) :: Float
    mh = logBase 2 (fromIntegral h) :: Float


makeBufferTextureImpl :: MonadIO m => TextureResidency -> V2 GLsizei -> GLenum -> GLint -> m Texture
makeBufferTextureImpl resident sz@(V2 w h) fmt mips = do
  tex <- allocateObject (glGenTextures 1)
  let target = GL_TEXTURE_2D
      mips' = max 1 . min (maxMip sz) $ mips
  glBindTexture target tex
  glTexStorage2D target mips' fmt w h
  glTexParameteri target GL_TEXTURE_BASE_LEVEL 0
  glTexParameteri target GL_TEXTURE_MAX_LEVEL mips'
  glTexParameteri target GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
  glTexParameteri target GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri target GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri target GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
  hdl <- texResident tex resident
  glBindTexture target 0
  pure (Texture target tex hdl fmt sz)

-- | Make a low level buffer texture, resident
makeBufferTexture :: MonadIO m => V2 GLsizei -> GLenum -> GLint -> m Texture
makeBufferTexture = makeBufferTextureImpl Resident

-- | Make a low level buffer texture, non-resident
makeBufferTextureNonResident :: MonadIO m => V2 GLsizei -> GLenum -> GLint -> m Texture
makeBufferTextureNonResident = makeBufferTextureImpl NonResident


-- | Actually just deletes the old texture and creates a new one.
--
-- As a result, could do *bad things* if the original texture or texture handle
-- are still alive elsewhere
unsafeResizeBufferTexture :: MonadIO m => V2 GLsizei -> GLint -> Texture -> m Texture
unsafeResizeBufferTexture sz mips tex = do
  let fmt = _textureFormat tex
  deleteTexture tex
  makeBufferTexture sz fmt mips
