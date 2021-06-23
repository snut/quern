module Quern.Codec.DDS
  (
  -- * Texture loading
    textureFromDDS
  -- * Re-exports
  , Tex.Texture(..)
  ) where

import Graphics.GL.Core45
import Graphics.GL.Types
import Graphics.GL.Ext.ARB.BindlessTexture
import qualified Data.Vector.Storable as VS
import Foreign
import Quern.Render.Texture as Tex
import Linear (V2(..))

import Quern.Codec.DDS.Internal

textureFromDDS :: FilePath -> Bool -> IO (Either String Texture)
textureFromDDS path makeResident = do
  ddsE <- loadDDS path
  case ddsE of
    Left e -> pure (Left e)
    Right dds -> case _ddsFileHeader10 dds of
      Just (DDSHeader10 dxgi _dim _flag _array _flag2) -> case dxgi of
        DXGI_FORMAT_BC7_UNORM -> ddsToBC7 dds makeResident Linear
        DXGI_FORMAT_BC7_UNORM_SRGB -> ddsToBC7 dds makeResident SRGB
        DXGI_FORMAT_BC6H_UF16 -> ddsToBC6 dds makeResident Unsigned
        DXGI_FORMAT_BC6H_SF16 -> ddsToBC6 dds makeResident Signed
        _ -> pure (Left $ "Unsupported DXGI format: " ++ show dxgi)
      _ -> pure (Left "Legacy DDS format: NYI")

{-# INLINE roundToBlock #-}
roundToBlock :: Integral a => a -> a
roundToBlock x = 4 * ((x + 3) `div` 4)

data ColourSpace = Linear | SRGB deriving (Eq, Ord, Enum, Bounded, Show, Read)
data Signed = Signed | Unsigned deriving (Eq, Ord, Enum, Bounded, Show, Read)

compressedTexBPTC :: VS.Vector Word8 -> GLint -> GLenum -> GLint -> GLint -> Bool -> IO (Either String Texture)
compressedTexBPTC bytes byteCount fmt w h makeResident
  | VS.length bytes < fromIntegral byteCount = pure (Left "Insufficient bytes in DDS file payload")
  | otherwise = do
    tex <- alloca $ \ptr -> glGenTextures 1 ptr *> peek ptr
    glBindTexture GL_TEXTURE_2D tex
    VS.unsafeWith bytes $ glCompressedTexImage2D GL_TEXTURE_2D 0 fmt w h 0 byteCount . castPtr
    -- NB: should use the actual mips if they exist
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0
    hdl <- if makeResident
      then do
        hdl <- glGetTextureHandleARB tex
        glMakeTextureHandleResidentARB hdl
        pure hdl
      else pure 0
    glBindTexture GL_TEXTURE_2D 0
    pure . Right $ Texture
      { _textureTarget = GL_TEXTURE_2D
      , _textureObject = tex
      , _textureHandle = hdl
      , _textureFormat = fmt
      , _textureSize = V2 w h }

ddsToBC7 :: DDSFile -> Bool -> ColourSpace -> IO (Either String Texture)
ddsToBC7 (DDSFile _ _ Nothing _) _ _ = pure (Left "Unsupported DDS format")
ddsToBC7 (DDSFile _ hdr (Just _hdr10) (DDSPayload bytes)) makeResident space
  | _headerCaps2 hdr /= Caps2'Simple = pure (Left "Unsupported texture type (cube, volume, array...)")
  | otherwise = compressedTexBPTC bytes byteCount fmt w h makeResident
  where
    w = fromIntegral (_headerWidth hdr)
    h = fromIntegral (_headerHeight hdr)
    -- BC7 uses one byte per pixel, using 4x4 blocks
    byteCount = roundToBlock w * roundToBlock h
    fmt = case space of
                Linear -> GL_COMPRESSED_RGBA_BPTC_UNORM
                SRGB   -> GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM


ddsToBC6 :: DDSFile -> Bool -> Signed -> IO (Either String Texture)
ddsToBC6 (DDSFile _ _ Nothing _) _ _ = pure (Left "Unsupported DDS format")
ddsToBC6 (DDSFile _ hdr (Just _hdr10) (DDSPayload bytes)) makeResident signed
  | _headerCaps2 hdr /= Caps2'Simple = pure (Left "Unsupported texture type (cube, volume, array...)")
  | otherwise = compressedTexBPTC bytes byteCount fmt w h makeResident {-do
    tex <- alloca $ \ptr -> glGenTextures 1 ptr *> peek ptr
    glBindTexture GL_TEXTURE_2D tex
    if VS.length bytes < fromIntegral byteCount
      then pure (Left "Insufficient bytes in DDS file payload")
      else do
        let fmt = case signed of
                    Signed   -> GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT
                    Unsigned -> GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT
        VS.unsafeWith bytes $ glCompressedTexImage2D GL_TEXTURE_2D 0 fmt w h 0 byteCount . castPtr
        -- NB: should use the actual mips if they exist
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0
        hdl <- if makeResident
          then do
            hdl <- glGetTextureHandleARB tex
            glMakeTextureHandleResidentARB hdl
            pure hdl
          else pure 0
        glBindTexture GL_TEXTURE_2D 0
        pure . Right $ Texture
          { _textureTarget = GL_TEXTURE_2D
          , _textureObject = tex
          , _textureHandle = hdl
          , _textureFormat = fmt}-}
  where
    w = fromIntegral (_headerWidth hdr)
    h = fromIntegral (_headerHeight hdr)
    -- BC6 uses one byte per pixel, using 4x4 blocks
    byteCount = roundToBlock w * roundToBlock h
    fmt = case signed of
                Signed   -> GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT
                Unsigned -> GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT
