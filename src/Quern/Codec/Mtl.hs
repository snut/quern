{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
-- {-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language DeriveGeneric #-}
{-# Language MultiWayIf #-}

module Quern.Codec.Mtl
  ( parseMtlFile
  , MaterialEx(..)
  ) where


import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Semigroup (Option(..), Last(..))
import Control.Monad.State
import Control.Lens hiding (noneOf, (<.>))
import Quern.Codec.Combinators
import Text.Parsec
import Linear hiding (trace)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as B

import System.FilePath.Posix as Path

import GHC.Generics (Generic)

import Quern.Render.Opacity

--import Debug.Trace
import Quern.Render.StaticScene.SceneStorage

{-
https://en.wikipedia.org/wiki/Wavefront_.obj_file#Material_template_library


newmtl Textured
Ka 1.000 1.000 1.000
Kd 1.000 1.000 1.000
Ks 0.000 0.000 0.000
d 1.0
illum 2
map_Ka lemur.tga           # the ambient texture map
map_Kd lemur.tga           # the diffuse texture map (most of the time, it will
                           # be the same as the ambient texture map)
map_Ks lemur.tga           # specular color texture map
map_Ns lemur_spec.tga      # specular highlight component
map_d lemur_alpha.tga      # the alpha texture map
map_bump lemur_bump.tga    # some implementations use 'map_bump' instead of 'bump' below

-}


data Illum
  = NoAmbient
  | Ambient
  | Highlight
  | ReflectionRaytrace
  | TransparencyGlassReflectionRaytrace
  | ReflectionFresnelRaytrace
  | TransparencyRefractionRaytrace
  | TransparencyRefractionFresnelRaytrace
  | Reflection
  | TransparencyGlassReflection
  | CastShadowsOnInvisible
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

isMetal :: Illum -> Bool
isMetal ReflectionRaytrace = True
isMetal Reflection = True
isMetal ReflectionFresnelRaytrace = True
isMetal _ = False

isTransparent :: Illum -> Bool
isTransparent TransparencyGlassReflectionRaytrace = True
isTransparent TransparencyRefractionRaytrace = True
isTransparent TransparencyRefractionFresnelRaytrace = True
isTransparent TransparencyGlassReflection = True
isTransparent _ = False


data ObjMtlTexture = ObjMtlTexture
  { _mtlTexPath :: !FilePath
  , _mtlTexOptions :: ![String]
  } deriving (Eq, Ord, Generic)
makeLenses ''ObjMtlTexture

instance Show ObjMtlTexture where
  showsPrec _ (ObjMtlTexture [] _) = showString "NoTex"
  showsPrec d (ObjMtlTexture fp os) = showParen (d > 10) $ showString ("Tex " <> fp <> " " <> show os)

emptyMtlTexture :: ObjMtlTexture
emptyMtlTexture = ObjMtlTexture "" []

data MtlChannel a = MtlChannel
  { _mtlChScalar :: !(Maybe a)
  , _mtlChTexture :: !ObjMtlTexture
  } deriving (Eq, Ord, Generic)
makeLenses ''MtlChannel

instance Show a => Show (MtlChannel a) where
  showsPrec d (MtlChannel Nothing (ObjMtlTexture [] _)) = showParen (d > 10) $ showString "NoCh"
  showsPrec d (MtlChannel (Just s) (ObjMtlTexture [] _)) = showParen (d > 10) $ showString "Ch " . showsPrec 11 s
  showsPrec d (MtlChannel _ tx@(ObjMtlTexture (_:_) _)) = showParen (d > 10) $ showString "Tx " . showsPrec 11 tx
  --showsPrec d (MtlChannel Nothing x) = showParen (d > 10) $ showString "Ch " . showsPrec 11 x
  --showsPrec d (MtlChannel (Just s) x) = showParen (d > 10) $ showString "Ch " . showsPrec 11 s . showString " " . showsPrec 11 x

lastMB :: Maybe a -> Maybe a -> Maybe a
lastMB x y = fmap getLast . getOption $ Option (Last <$> x) <> Option (Last <$> y)

instance Semigroup (MtlChannel a) where
  (MtlChannel sc0 tx0@(ObjMtlTexture p0 _)) <> (MtlChannel sc1 tx1@(ObjMtlTexture p1 _)) = MtlChannel
    { _mtlChScalar = lastMB sc0 sc1
    , _mtlChTexture = if null p0 then tx1 else (if null p1 then tx0 else tx1)
    }
instance Monoid (MtlChannel a) where
  mempty = emptyMtlChannel

dfltMtlChannel :: a -> MtlChannel a
dfltMtlChannel x = MtlChannel (Just x) emptyMtlTexture

emptyMtlChannel :: MtlChannel a
emptyMtlChannel = MtlChannel Nothing emptyMtlTexture

data MtlDefn = MtlDefn
  { _mtlKa :: !(MtlChannel (V3 Float)) -- ambient colour
  , _mtlKd :: !(MtlChannel (V3 Float)) -- diffuse colour
  , _mtlKs :: !(MtlChannel (V3 Float)) -- specular colour
  , _mtlNs :: !(MtlChannel Float) -- specular exponent, 0 .. 1000
  , _mtlNi :: !(MtlChannel Float) -- ior?
  , _mtlTr :: !Float -- 'dissolve' = d ~ alpha - may also be present as 'Tr', for transparency
  , _mtlIllum :: !Illum
  , _mtlBump :: !ObjMtlTexture -- might also be just 'bump'
  , _mtlDisp :: !ObjMtlTexture -- displacement 'disp'
  , _mtlDecal :: !ObjMtlTexture -- 'decal' - a stencil decal?
  , _mtlRefl :: !ObjMtlTexture
  -- PBR extensions a la Clara.io
  , _mtlPr :: !((MtlChannel Float)) -- roughness
  , _mtlPm :: !((MtlChannel Float)) -- metallic
  , _mtlPs :: !((MtlChannel Float)) -- sheen
  , _mtlKe :: !((MtlChannel (V3 Float))) -- emmisive
  , _mtlPc :: !(Maybe Float) -- clearcoat thickness
  , _mtlPcr :: !(Maybe Float) -- clearcoat roughness
  , _mtlAniso :: !(Maybe Float) -- anisotropy
  , _mtlAnisoR :: !(Maybe Float) -- aniso rotation
  , _mtlNorm :: !(Maybe ObjMtlTexture) -- normal (rather than bump) map
  , _mtlRMA :: !(Maybe ObjMtlTexture) -- roughness, metallic, AO
  , _mtlORM :: !(Maybe ObjMtlTexture) -- ao, roughness, metallic
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''MtlDefn


newtype MtlLibrary = MtlLibrary { _mtlLibMap :: HashMap String MtlDefn } deriving (Show)
makeLenses ''MtlLibrary

emptyMtlDefn :: MtlDefn
emptyMtlDefn = MtlDefn
  { _mtlKa = dfltMtlChannel 1
  , _mtlKd = dfltMtlChannel 1
  , _mtlKs = dfltMtlChannel 1
  , _mtlNs = dfltMtlChannel 500
  , _mtlNi = dfltMtlChannel 1
  , _mtlTr = 0
  , _mtlIllum = Ambient
  , _mtlBump = emptyMtlTexture
  , _mtlDisp = emptyMtlTexture
  , _mtlDecal = emptyMtlTexture
  , _mtlRefl = emptyMtlTexture
  -- PBR
  , _mtlPr = emptyMtlChannel
  , _mtlPm = emptyMtlChannel
  , _mtlPs = emptyMtlChannel
  , _mtlKe = emptyMtlChannel
  , _mtlPc = Nothing
  , _mtlPcr = Nothing
  , _mtlAniso = Nothing
  , _mtlAnisoR = Nothing
  , _mtlNorm = Nothing
  , _mtlRMA = Nothing
  , _mtlORM = Nothing
  }

data MtlState = MtlState
  { _mtlStateName :: !String
  , _mtlStateDefn :: !MtlDefn
  , _mtlStateLib :: !MtlLibrary
  , _mtlStateComments :: ![String]
  } deriving (Show, Generic)
makeLenses ''MtlState

type MtlBuilder a = ParsecT B.ByteString () (StateT MtlState Identity) a

str :: Stream s m Char => ParsecT s u m String
str = many1 (noneOf " \t\n\r#")

nameStr :: Stream s m Char => ParsecT s u m String
nameStr = lexeme str

fixPath :: FilePath -> FilePath
fixPath [] = []
fixPath ('\\':'\\':cs) = '/' : fixPath cs
fixPath ('\\':cs) = '/' : fixPath cs
fixPath (c:cs) = c : fixPath cs

texture :: Stream s m Char => ParsecT s u m ObjMtlTexture
texture = do
  xs <- sepBy str hspaces1
  case xs of
    [] -> pure emptyMtlTexture
    _ -> pure $ ObjMtlTexture (fixPath $ last xs) (init xs)

mtlParser :: MtlBuilder MtlState
mtlParser = do
  _ <- endBy mtlLine end
  newMtl "__EOF__"
  get

ensureM :: MonadState s m => Lens' s (Maybe a) -> a -> m ()
ensureM l dflt = do
  x <- use l
  case x of
    Just _ -> pure ()
    Nothing -> l .= Just dflt

data MaterialEx a = MaterialEx
  { _materialExMtl :: !a
  , _materialExOpacity :: !Opacity
  } deriving (Eq, Ord, Show, Generic)

parseMtlDefns :: MonadIO m => FilePath -> m (Either ParseError MtlLibrary)
parseMtlDefns path = liftIO $ fmap _mtlStateLib .  mtlLib path <$> B.readFile path

parseMtlFile :: MonadIO m => FilePath -> m (HashMap String (MaterialEx MaterialPathsFallback))
parseMtlFile path = do
  result <- parseMtlDefns path --liftIO $ mtlLib path <$> B.readFile path
  case result of
    Right (MtlLibrary lib) -> do
      pure $ mtlDefnToMaterial <$> lib
    Left _ -> pure mempty

mtlLib :: String -> B.ByteString -> Either ParseError MtlState
mtlLib srcName bs = runIdentity $ evalStateT (runParserT mtlParser () srcName bs) st
  where st = MtlState "" emptyMtlDefn (MtlLibrary mempty) []

prefixPr :: String -> MtlBuilder a -> Lens' MtlDefn (MtlChannel a) -> MtlBuilder ()
prefixPr s p l
   =  (prefixed s p >>= (mtlStateDefn . l <>=) . dfltMtlChannel)
  <|> (prefixed ("map_" <> s) texture >>= (mtlStateDefn . l . mtlChTexture .=))


prefixCh :: String -> MtlBuilder a -> Lens' MtlDefn (MtlChannel a) -> MtlBuilder ()
prefixCh s p l
   =  (prefixed s p >>= (mtlStateDefn . l . mtlChScalar .=) . Just)
  <|> (prefixed ("map_" <> s) texture >>= (mtlStateDefn . l . mtlChTexture .=))

mtlLine :: MtlBuilder ()
mtlLine
   =  (prefixed "newmtl" nameStr >>= newMtl)
  <|> (prefixCh "Kd"     vec3  mtlKd)
  <|> (prefixCh "Ka"     vec3  mtlKa)
  <|> (prefixCh "Ks"     vec3  mtlKs)
  <|> (prefixCh "Ns"     float mtlNs)
  <|> (prefixCh "Ni"     float mtlNi)
  <|> (prefixed "d"      float >>= (mtlStateDefn . mtlTr .=) . (1 -))
  <|> (prefixed "Tr"     float >>= (mtlStateDefn . mtlTr .=))
  <|> (prefixed "illum"  int   >>= (mtlStateDefn . mtlIllum .=) . toEnum . fromIntegral)
  <|> (prefixPr "Pr"     float mtlPr)
  <|> (prefixPr "Pm"     float mtlPm)
  <|> (prefixPr "Ps"     float mtlPs)
  <|> (prefixPr "Ke"     vec3  mtlKe)
  <|> (prefixed "Pc"     float >>= (mtlStateDefn . mtlPc .=) . Just)
  <|> (prefixed "Pcr"    float >>= (mtlStateDefn . mtlPcr .=) . Just)
  <|> (prefixed "aniso"  float >>= (mtlStateDefn . mtlAniso .=) . Just)
  <|> (prefixed "anisor" float >>= (mtlStateDefn . mtlAnisoR .=) . Just)
  <|> (prefixed "#" restOfLine >>= (mtlStateComments <>=) . (:[]))
  <|> (prefixed "map_Bump" texture >>= (mtlStateDefn . mtlBump .=))
  -- <|> (string "" *> pure ())
  <?> "valid .MTL line"

newMtl :: String -> MtlBuilder ()
newMtl nm = do
  currName <- use mtlStateName
  currDefn <- use mtlStateDefn
  when (not (null currName)) $
    mtlStateLib . mtlLibMap %= HM.insert currName currDefn
  mtlStateName .= nm
  mtlStateDefn .= emptyMtlDefn

orPath :: FilePath -> FilePath -> FilePath
orPath [] b = b
orPath a  _ = a

swapSuffix :: String -> String -> FilePath -> FilePath
swapSuffix old new path
  | null path = path
  | otherwise = dir </> bn' <.> ext
  where
    ext = takeExtension path
    bn = takeBaseName path
    dir = takeDirectory path
    (pref,suff) = splitAt (length bn - length old) bn
    bn' = if suff == old then pref<>new else bn

mtlDefnToMaterial :: MtlDefn -> MaterialEx MaterialPathsFallback
mtlDefnToMaterial defn = MaterialEx mtl opacity
  where
    opacity =
      if
        | isTransparent illum -> Transparent
        | "_masked_" `L.isInfixOf` bcTx -> Masked
        | otherwise -> Opaque
    V3 r g b =  linToSRGB <$> (fromMaybe 0.8 $ defn ^. mtlKd . mtlChScalar)
    illum = defn ^. mtlIllum
    alpha = 1 - defn ^. mtlTr
    bc = fToByte <$> V4 r g b alpha
    nrm = V4 128 128 255 255
    armh = V4 255 rough metal 127
    rough = fToByte $ fromMaybe (fromMaybe 1 (specToR <$> defn ^. mtlNs . mtlChScalar)) (defn ^. mtlPr . mtlChScalar)
    metal = fToByte $ fromMaybe (if isMetal illum then 255 else 0) (defn ^. mtlPm . mtlChScalar)
    specToR = (1 -) . sqrt . (/ 1000)
    fToByte = round . (* 255) . min 1 . max 0
    linToSRGB = sqrt -- not exactly...
    bcTx = defn^.mtlKd.mtlChTexture.mtlTexPath
    armhTx = swapSuffix "_basecolor" "_armh" bcTx
    nrmTx = defn^.mtlBump.mtlTexPath
    emitTx = swapSuffix "_basecolor" "_emit" bcTx
    V3 er eg eb = linToSRGB <$> fromMaybe 0 (defn ^. mtlKe . mtlChScalar)
    emit = fToByte <$> V4 er eg eb 255
    mtl = Material
      { _materialBaseColour = (bcTx, bc)
      , _materialNormal = (nrmTx, nrm)
      , _materialAmbientRoughnessMetallicHeight = (armhTx, armh)
      , _materialSubsurfaceEmissive = (emitTx, emit) }






-- eof
