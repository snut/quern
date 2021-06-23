{-# Language DeriveGeneric #-}

module Quern.Font.Types
  ( -- * Basic types and functions
    TextQuads(..)
  , TextVertex(..)
  , textQuads
  , textQuadSize
  , guiRect
  --
  , TextStyle(..)
  , defaultTextStyle
  --
  , Font(..)
  , Info(..)
  , Common(..)
  , Page(..)
  , Character(..)
  , Percent(..)
  , ScreenRes
  ) where

import Data.Char (ord)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as VS
import Data.Text (Text)
import Data.ByteString (ByteString)
import Foreign
import GHC.Generics (Generic)
import Linear

import Quern.Physics.AxisAlignedBox

newtype Percent = Pct Int deriving (Eq, Ord, Show, Read)

data Info = Info
  { _infoFace :: !Text
  , _infoSize :: !Int
  , _infoBold :: !Bool
  , _infoItalic :: !Bool
  , _infoCharset :: !(Maybe ByteString) -- if Nothing, font uses unicode charset
  , _infoStretchH :: !Percent
  , _infoSmooth :: !Bool
  , _infoAA :: !Int -- aa samples, 1 = no aa
  , _infoPadding :: !(V4 Int) -- up, right, down, left
  , _infoSpacing :: !(V2 Int) -- horizontal, vertical
  , _infoOutline :: !Int
  } deriving (Eq, Show)


data Common = Common
  { _commonLineHeight :: !Int -- pixels between lines of text
  , _commonBase :: !Int -- pixels from top of line to character base
  , _commonScale :: !(V2 Int) -- resolution of the texture
  , _commonPages :: !Int -- number of texture pages
  , _commonPacked :: !Bool -- packed texture
  , _commonChannels :: !(V4 Int) -- rgba meaning: 0 - glyph data, 1 - outline, 2 - glyph & outline, 3 - const 0, 4 - const 1
  } deriving (Eq, Show)

newtype Page = Page { _pageFile :: ByteString } deriving (Eq, Show)

data Character = Character
  { _charPos :: !(V2 Int) -- position in image
  , _charSize :: !(V2 Int) -- size in image
  , _charOffset :: !(V2 Int) -- offset to apply to character when displaying
  , _charAdvance :: !Int -- distance to advance cursor after displaying this character (x)
  , _charPage :: !Int -- page id
  , _charChannel :: !Int -- 1 = blue, 2 = green, 4 = red, 8 = alpha, 15 = all
  } deriving (Eq, Show)

data Font = Font
  { _fontInfo :: !Info
  , _fontCommon :: !Common
  , _fontPages :: !(IntMap Page)
  , _fontCharCount :: !Int
  , _fontChars :: !(IntMap Character)
  , _fontKerningCount :: !Int
  , _fontKerning :: !(IntMap (IntMap Int))
  } deriving (Eq, Show)



data TextStyle = TextStyle
  { _textStyleSize :: !Float
  , _textStyleClrFore :: !(V4 Word8)
  , _textStyleClrBack :: !(V4 Word8)
  } deriving (Eq, Ord, Show, Read, Generic)

defaultTextStyle :: TextStyle
defaultTextStyle = TextStyle 1.0 (pure 255) (pure 0)

-- slowly becoming a gui vertex...
data TextVertex = TextVertex !(V2 Float) !(V2 Float) !(V4 Word8) !(V4 Word8) !(V4 Word32)
  deriving (Eq, Ord, Show, Read, Generic)

instance Storable TextVertex where
  peek ptr = do
    p <- peek (castPtr ptr)
    t <- peek (ptr `plusPtr` 8)
    fg <- peek (ptr `plusPtr` 16)
    bg <- peek (ptr `plusPtr` 20)
    sdf <- peek (ptr `plusPtr` 24)
    pure $ TextVertex p t fg bg sdf
  poke ptr (TextVertex pos uv fg bg sdf) = do
    poke (castPtr ptr) pos
    poke (plusPtr ptr 8) uv
    poke (plusPtr ptr 16) fg
    poke (plusPtr ptr 20) bg
    poke (plusPtr ptr 24) sdf
  sizeOf _ = sizeOf (undefined :: V2 Float) * 2 + 8 + sizeOf (undefined :: V4 Int32)
  alignment _ = alignment (undefined :: V2 Float)

data TextQuads = TextQuads
  { _textQuadsVertices :: !(VS.Vector (V4 TextVertex))
  , _textQuadsBounds :: !(AABox V2 Int)
  } deriving (Eq, Show)

instance Semigroup TextQuads where
  (TextQuads as bxa) <> (TextQuads bs bxb) = TextQuads (as <> bs) (bxa <> bxb)
instance Monoid TextQuads where
  mempty = TextQuads mempty (AABox maxBound minBound)

textQuadSize :: Int
textQuadSize = sizeOf (undefined :: TextVertex) * 4

type ScreenRes = V2 Int

textQuads :: Font -> V2 Int -> TextStyle -> String -> TextQuads
textQuads font start style str = TextQuads vs bx
  where
    scale = _textStyleSize style
    start' = round <$> (fromIntegral <$> start) / pure scale
    (cs, bx) = charsOut font start' str
    vs = VS.fromList (toVerts (pure scale) style <$> cs)

kerningAdjust :: Font -> Char -> Maybe Char -> Int
kerningAdjust _font _c Nothing = 0
kerningAdjust font c (Just c') = case IM.lookup (ord c) (_fontKerning font) >>= IM.lookup (ord c') of
  Just x -> x
  Nothing -> 0

data CharOut = CharOut
  { _charOutOffset :: V2 Int -- offset from some fixed point (uniform character quad, or other anchor)
  , _charOutSize :: V2 Int -- size of character quad (px)
  , _charOutAdvance :: Int -- distance to start of next character, possibly with kerning
  , _charOutUVScaleBias :: V4 Float -- uv start and size
  } deriving (Eq, Ord, Show, Read)

-- Flip vertex positions in Y, as they are stored with Y increasing downwards
-- NB: This means positions for text are the top left point
flipY :: Num a => V2 a -> V2 a
flipY (V2 x y) = V2 x (negate y)

toVerts :: V2 Float -> TextStyle -> CharOut -> V4 TextVertex
toVerts textScale style chr = V4 a b c d
  where
    (V2 posX0 posY0) = textScale * (fromIntegral <$> _charOutOffset chr)
    (V2 posX1 posY1) = textScale * (fromIntegral <$> _charOutOffset chr + _charOutSize chr)
    (V4 su sv uvU0 uvV0) = _charOutUVScaleBias chr
    uvU1 = uvU0 + su
    uvV1 = uvV0 + sv
    fg = _textStyleClrFore style
    bg = _textStyleClrBack style
    sdf = pure 0
    a = TextVertex (V2 posX0 posY0) (V2 uvU0 uvV0) fg bg sdf
    c = TextVertex (V2 posX1 posY0) (V2 uvU1 uvV0) fg bg sdf
    b = TextVertex (V2 posX0 posY1) (V2 uvU0 uvV1) fg bg sdf
    d = TextVertex (V2 posX1 posY1) (V2 uvU1 uvV1) fg bg sdf

-- hmm
guiRect :: AABox V2 Int -> Int -> Int -> V4 Word8 -> V4 Word8 -> TextQuads
guiRect bx@(AABox bl@(V2 lf bt) tr@(V2 rt tp)) rnd sft fg bg = TextQuads vs bx
  where
    vs = VS.singleton $ V4 a b c d
    w32 :: Int -> Word32
    w32 = fromIntegral
    V2 posX0 posY0 = fromIntegral . (subtract sft) <$> bl
    V2 posX1 posY1 = fromIntegral . (+ sft) <$> tr
    bl' = w32 (lf.&.0xffff) .|. w32 ((bt.&.0xffff) `shiftL` 16)
    tr' = w32 (rt.&.0xffff) .|. w32 ((tp.&.0xffff) `shiftL` 16)
    rs  = w32 (rnd .&. 0xff) .|. w32 ((sft .&. 0xff) `shiftL` 8)
    sdf = V4 bl' tr' rs 0
    a = TextVertex (V2 posX0 posY0) 0 fg bg sdf
    b = TextVertex (V2 posX1 posY0) 0 fg bg sdf
    c = TextVertex (V2 posX0 posY1) 0 fg bg sdf
    d = TextVertex (V2 posX1 posY1) 0 fg bg sdf



placeCharOut :: V2 Int -> (CharOut, AABox V2 Int) -> (CharOut, AABox V2 Int)
placeCharOut p (ch, bx) = (ch{ _charOutOffset = _charOutOffset ch + p }, translate p bx)

charPosUV :: Font -> Char -> Maybe Char -> (CharOut, AABox V2 Int)
charPosUV font c next = case IM.lookup (ord c) (_fontChars font) of
    Just chr -> let co = mkChr chr in (co, mkBox co <> mkAABox 0 (V2 0 lineH))
    Nothing -> (CharOut 0 0 0 0, AABox maxBound minBound)
  where
    mkBox co = mkAABox 0 (_charOutSize co + _charOutOffset co + V2 (_charOutAdvance co) 0)
    mkChr chr = CharOut (flipY (_charOffset chr)) (flipY (_charSize chr)) (kerningAdjust font c next + _charAdvance chr) (scaleBias chr)
    lineH = _commonLineHeight (_fontCommon font)
    rcpRes = recip . fromIntegral <$> _commonScale (_fontCommon font)
    scaleBias chr = V4 sx sy px py
      where
        (V2 sx sy) = rcpRes * (fromIntegral <$> _charSize chr)
        (V2 px py) = rcpRes * (fromIntegral <$> _charPos chr)

consFst :: a -> ([a], b) -> ([a], b)
consFst a (as, b) = (a:as, b)

charsOut :: Font -> V2 Int -> String -> ([CharOut], AABox V2 Int)
charsOut font start str = go (degenerate start) str start start
  where
    lineH = _commonLineHeight (_fontCommon font)
    go bx [] _ _ = ([], bx)
    go bx ('\n':cs) _ (V2 lx ly) = let l' = V2 lx (ly - lineH) in go bx cs l' l'
    go bx ('\r':cs) p l = go bx cs p l
    go bx [c] p _ =
      let (ch,bx') = placeCharOut p (charPosUV font c Nothing)
      in ([ch], bx<>bx')
    go bx (c:cs@(c':_)) p l =
      let (ch,bx') = charPosUV font c (Just c')
          (ch',bx'') = placeCharOut p (ch, bx')
      in consFst ch' $ go (bx<>bx'') cs (p + V2 (_charOutAdvance ch) 0) l
