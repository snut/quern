{-# Language GeneralizedNewtypeDeriving #-}
module Quern.Codec.DDS.Internal
  (
  -- * Types
    DDSFile(..)
  , DDSHeader(..)
  , DDSPixelFormat(..)
  , DDSHeader10(..)
  , DDSResourceDimension(..)
  , DDSPayload(..)
  , DDSCaps1(..)
  , DDSCaps2(..)
  , FCC(..)
  -- * Functions
  , loadDDS
  , ddsPayloadSize
  -- * Re-exports
  , DXGIFormat(..)
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT, get, put, modify')
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Bits
import Data.Char (ord, chr)
import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word
import Foreign
import Linear
import System.IO.MMap
import Quern.Codec.DDS.DXGIFormat

-- | Four character code, treating each character as a simple byte and packing them into a 'Word32'
newtype FCC = FCC Word32 deriving (Eq, Ord)
instance Show FCC where
  showsPrec _ (FCC w) = showString ['"', a, b, c, d, '"']
    where
      x = fromIntegral w
      a = chr  (x .&. 0x000000ff)
      b = chr ((x .&. 0x0000ff00) `shiftR` 8)
      c = chr ((x .&. 0x00ff0000) `shiftR` 16)
      d = chr ((x .&. 0xff000000) `shiftR` 24)

-- | Convert a string to an FCC; truncated at four characters, padded with spaces.
fourCC :: String -> FCC
fourCC str = FCC . fromIntegral $ case str of
    (a:b:c:d:_) -> (ord' a 0) .|. (ord' b 8) .|. (ord' c 16) .|. (ord' d 24)
    [a,b,c]     -> (ord' a 0) .|. (ord' b 8) .|. (ord' c 16) .|. (ord' s 24)
    [a,b]       -> (ord' a 0) .|. (ord' b 8) .|. (ord' s 16) .|. (ord' s 24)
    [a]         -> (ord' a 0) .|. (ord' s 8) .|. (ord' s 16) .|. (ord' s 24)
    []          -> (ord' s 0) .|. (ord' s 8) .|. (ord' s 16) .|. (ord' s 24)
  where
    s = ' '
    ord' c n = ord c `shiftL` n

ddsMagic :: FCC
ddsMagic = FCC 0x20534444 -- fourCC "DDS "

data DDSFile = DDSFile
  { _ddsFileMagic :: {-# UNPACK #-} !FCC
  , _ddsFileHeader :: !DDSHeader
  , _ddsFileHeader10 :: !(Maybe DDSHeader10)
  , _ddsFilePayload :: !DDSPayload
  } deriving (Eq, Show)

-- size: 124 bytes
data DDSHeader = DDSHeader
  { _headerSize :: {-# UNPACK #-} !Word32 -- 124
  , _headerFlags :: !(S.Set DDSHeaderFlag)
  , _headerHeight :: {-# UNPACK #-} !Word32
  , _headerWidth :: {-# UNPACK #-} !Word32
  , _headerPitchLinSize :: {-# UNPACK #-} !Word32
  , _headerDepth :: {-# UNPACK #-} !Word32
  , _headerMipMapCount :: {-# UNPACK #-} !Word32
  , _headerReserved :: !(VS.Vector Word32) -- exactly 11
  , _headerPixelFormat :: !DDSPixelFormat
  , _headerCaps1 :: !(S.Set DDSCaps1)
  , _headerCaps2 :: !DDSCaps2
  , _headerCaps3 :: {-# UNPACK #-} !Word32
  , _headerCaps4 :: {-# UNPACK #-} !Word32
  , _headerReserved2 :: {-# UNPACK #-} !Word32
  } deriving (Eq, Show)

-- size: 32 bytes
data DDSPixelFormat = DDSPixelFormat
  { _pixelFormatSize :: {-# UNPACK #-} !Word32 -- 32
  , _pixelFormatFlags :: {-# UNPACK #-} !Word32
  , _pixelFormatFourCC :: !FCC
  , _pixelFormatRGBBitCount :: {-# UNPACK #-} !Word32
  , _pixelFormatRGBABitMask :: {-# UNPACK #-} !(V4 Word32)
  } deriving (Eq, Show)

ddpf'FourCC :: Word32
ddpf'FourCC = 0x4

fcc'dx10 :: FCC
fcc'dx10 = fourCC "DX10"

-- size: 20 bytes
data DDSHeader10 = DDSHeader10
  { _header10Format :: !DXGIFormat
  , _header10Dimension :: !DDSResourceDimension
  , _header10MiscFlag :: {-# UNPACK #-} !Word32
  , _header10ArraySize :: {-# UNPACK #-} !Word32
  , _header10MiscFlag2 :: {-# UNPACK #-} !Word32
  } deriving (Eq, Show)

data DDSResourceDimension = Dimension'1D | Dimension'2D | Dimension'3D | Dimension'Error
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

toDimension :: Word32 -> DDSResourceDimension
toDimension 2 = Dimension'1D
toDimension 3 = Dimension'2D
toDimension 4 = Dimension'3D
toDimension _ = Dimension'Error


newtype DDSPayload = DDSPayload (VS.Vector Word8) deriving (Eq)
instance Show DDSPayload where
  showsPrec d (DDSPayload bs) = showParen (d >= 10) (showString "DDSPayload " . showsPrec 0 (VS.length bs))

ddsPayloadSize :: DDSPayload -> Int
ddsPayloadSize (DDSPayload v) = VS.length v

data MappedFileState = MFS
  { _mfsPointer :: {-# UNPACK #-} !(Ptr Word32) -- ^ Position in file
  , _mfsSizeRem :: {-# UNPACK #-} !Int -- ^ Bytes left in file
  , _mfsRunningCount :: {-# UNPACK #-} !Int -- ^ Number of bytes read so far
  }

{-# INLINE nextMFS #-}
nextMFS :: MappedFileState -> MappedFileState
nextMFS (MFS p s c) = MFS (p `advancePtr` 1) (s - 4) (c + 4)

type LoadT = StateT MappedFileState (ExceptT String IO)

{-# INLINE word32 #-}
word32 :: LoadT Word32
word32 = do
  (MFS ptr size count) <- get
  if size <= 3
    then ddsErr ("DDS: unexpected EoF at " ++ show count) *> pure 0
    else do
      modify' nextMFS
      liftIO $ peek ptr

word32x4 :: LoadT (V4 Word32)
word32x4 = V4 <$> word32 <*> word32 <*> word32 <*> word32

payload :: LoadT (VS.Vector Word8)
payload = do
  (MFS p s c) <- get
  when (s <= 0) $ ddsErr ("DDS: unexpected EoF getting payload at " ++ show c)
  put (MFS nullPtr 0 (c + s))
  let src8 = castPtr p :: Ptr Word8
  liftIO $ do
    v <- VSM.unsafeNew s
    VSM.unsafeWith v $ \dst -> copyBytes dst src8 s
    VS.unsafeFreeze v

{-# INLINEABLE ddsErr #-}
ddsErr :: String -> LoadT ()
ddsErr = throwError


{-# INLINEABLE ddsAssert #-}
ddsAssert :: LoadT a -> (a -> Bool) -> String -> LoadT a
ddsAssert act prd msg = do
  a <- act
  if prd a
    then pure a
    else ddsErr msg *> pure a

loadDDS :: FilePath -> IO (Either String DDSFile)
loadDDS path = mmapWithFilePtr path ReadOnly Nothing $ \(ptr, size) -> runExceptT $ flip evalStateT (MFS (castPtr ptr) size 0) $ do
  magic <- FCC <$> word32
  when (magic /= ddsMagic) $ ddsErr ("DDS: magic mismatch, first four bytes should be \"DDS \" but found " ++ show magic)
  hdr <- loadHeader
  let pf = _headerPixelFormat hdr
  hdr10 <- if _pixelFormatFlags pf == ddpf'FourCC && _pixelFormatFourCC pf == fcc'dx10
            then Just <$> loadHeader10
            else pure Nothing
  bulk <- payload
  pure $ DDSFile magic hdr hdr10 (DDSPayload bulk)

loadHeader :: LoadT DDSHeader
loadHeader =
  DDSHeader
  <$> ddsAssert word32 (== 124) "DDS: header size should be 124"
  <*> (toFlags <$> word32)
  <*> word32
  <*> word32
  <*> word32
  <*> word32
  <*> word32
  <*> VS.replicateM 11 word32
  <*> loadPixelFormat
  <*> (toCaps1 <$> word32)
  <*> (toCaps2 <$> word32)
  <*> word32
  <*> word32
  <*> word32

loadPixelFormat :: LoadT DDSPixelFormat
loadPixelFormat =
  DDSPixelFormat
  <$> ddsAssert word32 (== 32) "DDS: pixel format size should be 32"
  <*> word32
  <*> (FCC <$> word32)
  <*> word32
  <*> word32x4

loadHeader10 :: LoadT DDSHeader10
loadHeader10 =
  DDSHeader10
  <$> (toEnum . fromIntegral <$> word32)
  <*> (toDimension <$> word32)
  <*> word32
  <*> word32
  <*> word32

data DDSHeaderFlag
  = Flag'Caps
  | Flag'Height
  | Flag'Width
  | Flag'Pitch
  | Flag'PixelFormat
  | Flag'MipMapCount
  | Flag'LinearSize
  | Flag'Depth
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

flagValues :: [(Word32, DDSHeaderFlag)]
flagValues =
  [ (0x1, Flag'Caps)
  , (0x2, Flag'Height)
  , (0x4, Flag'Width)
  , (0x8, Flag'Pitch)
  , (0x1000, Flag'PixelFormat)
  , (0x20000, Flag'MipMapCount)
  , (0x80000, Flag'LinearSize)
  , (0x800000, Flag'Depth)
  ]

-- | Adds something to a set if its associated bits are true
{-# INLINEABLE addIfSet #-}
{-# SPECIALIZE addIfSet :: Ord b => Word32 -> S.Set b -> (Word32,b) -> S.Set b #-}
addIfSet :: (Bits a, Ord b) => a -> S.Set b -> (a, b) -> S.Set b
addIfSet x flagSet (mask, flag)
  | mask == (mask .&. x) = S.insert flag flagSet
  | otherwise = flagSet

--{-# INLINE toFlags #-}
toFlags :: Word32 -> S.Set DDSHeaderFlag
toFlags x = foldl' (addIfSet x) mempty flagValues

data DDSCaps1 = Caps1'Complex | Caps1'MipMap | Caps1'Texture
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

caps1Values :: [(Word32, DDSCaps1)]
caps1Values = [(0x8, Caps1'Complex), (0x400000, Caps1'MipMap), (0x1000, Caps1'Texture)]

--{-# INLINE toCaps1 #-}
toCaps1 :: Word32 -> S.Set DDSCaps1
toCaps1 x = foldl' (addIfSet x) mempty caps1Values

data DDSCaps2 = Caps2'Simple | Caps2'CubeMap | Caps2'Volume | Caps2'Invalid
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

caps2CubeMap, caps2Volume :: Word32
caps2CubeMap = 0x200 .|. 0x400 .|. 0x800 .|. 0x1000 .|. 0x2000 .|. 0x4000 .|. 0x8000
caps2Volume = 0x200000

--{-# INLINE toCaps2 #-}
toCaps2 :: Word32 -> DDSCaps2
toCaps2 w
  | w == 0 = Caps2'Simple
  | (w .&. caps2CubeMap) == caps2CubeMap = Caps2'CubeMap
  | (w .&. caps2Volume) == caps2Volume = Caps2'Volume
  | otherwise = Caps2'Invalid
