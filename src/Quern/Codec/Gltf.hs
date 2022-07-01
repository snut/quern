{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
module Quern.Codec.Gltf where

-- for gl types
import Graphics.GL.Core45

import qualified Codec.GLB as GLB
import qualified Codec.GlTF as GlTF
import Codec.GlTF (GlTF(..))
import Codec.GlTF.Prelude (Size)
import qualified Codec.GlTF.Buffer as Bu
import qualified Codec.GlTF.BufferView as Bv
import qualified Codec.GlTF.Accessor as Ac
import qualified Codec.GlTF.Animation as An
import qualified Codec.GlTF.Node as No
import qualified Codec.GlTF.Material as Mt
import qualified Codec.GlTF.Mesh as Me

import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word

import Linear

import Quern.Render.StaticScene.SceneStorage
import Quern.Codec.MeshTypes

newtype MayT m a = MayT { runMayT :: m (Maybe a) }
instance Functor m => Functor (MayT m) where
  fmap f (MayT m) = MayT (fmap f <$> m)
instance Applicative m => Applicative (MayT m) where
  pure a = MayT $ pure (Just a)
  (MayT f) <*> (MayT x) = MayT $ (<*>) <$> f <*> x
instance Monad m => Monad (MayT m) where
  a >>= k = MayT $ do
    ja <- runMayT a
    case ja of
      Just a' -> runMayT $ k a'
      Nothing -> pure Nothing

(?!) :: Applicative m => V.Vector a -> Int -> MayT m a
v ?! ix = MayT . pure $ v V.!? ix

mayT :: Applicative m => Maybe a -> MayT m a
mayT = MayT . pure

mayFail :: Applicative m => MayT m a
mayFail = MayT $ pure Nothing


data GltfMesh = GltfMesh
  { _gltfMeshVertices :: !(VS.Vector StaticVertex)
  , _gltfMeshIndices :: !(V.Vector (VS.Vector Word32))
  -- , _gltfMeshSections :: !(V.Vector Mtls)
  -- skinning weights and indices
  , _gltfMeshSkinningIndices :: !(VS.Vector (V4 Word8))
  , _gltfMeshSkinningWeights :: !(VS.Vector (V4 Word8))
  -- , _gltfMeshAnimations :: !(V.Vector ???)
  } deriving (Eq, Ord, Show)

-- gltf files can contain multiple instances
-- this would be very useful for:
--   - level creation
--   - providing exemplar for WFC etc.
-- gltf files can also contain multiple meshes
fromGltfFile :: MonadIO m => FilePath -> m (Either String (V.Vector GltfMesh))
fromGltfFile path = error "fromGltfFile"

buildSimpleVertices :: GlTF -> VS.Vector ObjVertex
buildSimpleVertices = error "buildSimpleVertices"


data Chunk
  = RawData GLB.Chunk
  | Decoded GlTF
  deriving (Eq)

instance Show Chunk where
  showsPrec d (RawData ch) = showParen (d >= 10) $ showString "RawData " . shows (GLB.chunkLength ch)
  showsPrec d (Decoded tf) = showParen (d >= 10) $ showString "Decoded " . shows tf


deChunk chunk
  | GLB.chunkLength chunk == 0 = RawData chunk
  | otherwise = case GlTF.fromChunk chunk of
      Left _ -> RawData chunk
      Right gltf -> Decoded gltf

test = do
  GLB.fromFile "./data/meshes/skinned/pumpkin.glb" >>= \case
    Left err -> print err *> pure Nothing
    Right glb -> do
      let hdr = GLB.header glb
          chs = GLB.chunks glb
      pure $ Just $ fmap deChunk chs

{-
-- partial af
testGltf = do
  Just cs <- test
  let Decoded gltf = cs V.! 0
      RawData  raw = cs V.! 1
  pure (gltf, raw)

testMsh = do
  (gltf, chnk) <- testGltf
  let Just as = accessors gltf
      raw = GLB.chunkData chnk
  Just ms <- runMayT $ do
    streams <- V.mapM (makeDataStreamM gltf raw) as
    ms <- mayT (meshes gltf)
    pure $ fmap (makeMesh streams) ms
  pure ms
-}

-- accessors (vertex data, anim tracks)
data AttribType = Vec1 | Vec2 | Vec3 | Vec4 | Mat4 deriving (Eq, Ord, Show, Read, Enum, Bounded)
data CompType = Float | UShort | UByte | UInt deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DataStream = DataStream
  { _dataStreamType :: AttribType
  , _dataStreamComponent :: CompType
  , _dataStreamPayload :: (VS.Vector Word8)
  } deriving (Eq, Ord)

instance Show DataStream where
  showsPrec d (DataStream ty cmp pl) = showParen (d >= 10) $
    showString "DataStream " .
    shows ty .
    showString " " .
    shows cmp .
    showString " " .
    shows (VS.length pl)

attribType :: Ac.AttributeType -> Maybe AttribType
attribType (Ac.AttributeType t)
  | t == "SCALAR" = Just Vec1
  | t == "VEC4" = Just Vec4
  | t == "VEC3" = Just Vec3
  | t == "VEC2" = Just Vec2
  | t == "MAT4" = Just Mat4
  | otherwise   = Nothing

compType :: Ac.ComponentType -> Maybe CompType
compType (Ac.ComponentType cmp)
  | cmp == GL_FLOAT = Just Float
  | cmp == GL_UNSIGNED_BYTE = Just UByte
  | cmp == GL_UNSIGNED_SHORT = Just UShort
  | cmp == GL_UNSIGNED_INT = Just UInt
  | otherwise = Nothing



makeDataStreamM :: Monad m => GlTF -> BS.ByteString -> Ac.Accessor -> MayT m DataStream
makeDataStreamM gltf raw (Ac.Accessor cmpType _norml byteOff _count attType bview _min _max _sparse _name _extensions _extra) = do
  (Bv.BufferViewIx bvIx) <- mayT bview
  bufferViewsV <- mayT (bufferViews gltf)
  (Bv.BufferView (Bu.BufferIx buIx) bvOffset bvLen _stride _target _nameBv _extensBv _extrasBv) <- bufferViewsV ?! bvIx
  --bufferV <- mayT (buffers gltf)
  --(Bu.Buffer _byteLen _uri _nameBu _extensBu _extrasBu) <- bufferV ?! buIx
  ctype <- mayT $ compType cmpType
  atype <- mayT $ attribType attType
  if buIx == 0 && byteOff == 0 -- not good conventions, but conventions nontheless
    then do
      let vec = VS.fromList . take bvLen . BS.unpack . BS.drop bvOffset $ raw
      pure $ DataStream atype ctype vec
    else
      mayFail

data PrimMode = Triangles deriving (Eq, Ord, Show, Read, Enum, Bounded)
data MeshPrim = MeshPrim
  { _meshPrimAttribs :: HM.HashMap Text DataStream
  , _meshPrimIndices :: DataStream
  , _meshPrimMode :: PrimMode
  , _meshPrimMaterial :: ()
  } deriving (Eq, Show)

primMode :: Me.MeshPrimitiveMode -> Maybe PrimMode
primMode (Me.MeshPrimitiveMode m)
  | m == GL_TRIANGLES = Just Triangles
  | otherwise = Nothing

makeMeshPrim ::  V.Vector DataStream -> Me.MeshPrimitive -> Maybe MeshPrim
makeMeshPrim streams (Me.MeshPrimitive atts mode ixs _mtl _targets _exts _extras)  = do
  as <- traverse (\(Ac.AccessorIx ix) -> streams V.!? ix) atts
  pm <- primMode mode
  ix <- ixs >>= (streams V.!?) . Ac.unAccessorIx
  pure $ MeshPrim{_meshPrimAttribs = as, _meshPrimMode = pm, _meshPrimIndices = ix, _meshPrimMaterial = ()}

data IntermediateMesh = IntermediateMesh Text (V.Vector MeshPrim) deriving (Eq, Show)

makeMesh :: V.Vector DataStream -> Me.Mesh -> IntermediateMesh
makeMesh streams (Me.Mesh prims _weights name _extensions _extras) = IntermediateMesh n ps
  where
    ps = V.concatMap g prims
    g p = case makeMeshPrim streams p of
            Just mp -> V.singleton mp
            Nothing -> mempty
    n = case name of
          Just nm -> nm
          Nothing -> "?UNKNOWN?"

makeVertices :: IntermediateMesh -> V.Vector (VS.Vector ObjVertex, VS.Vector Word32)
makeVertices (IntermediateMesh _name prims) = fmap mkVs prims
  where
    mkVs (MeshPrim attr idx _mode _mtl) = (interleaveVts, idx')
      where
        ixp = _dataStreamPayload idx
        idx' = case _dataStreamComponent idx of
                UByte -> VS.map fromIntegral ixp
                UShort -> VS.map fromIntegral (VS.unsafeCast ixp :: VS.Vector Word16)
                UInt -> VS.unsafeCast ixp
        interleaveVts = VS.zipWith4 ObjVertex pos nrm tex clr
        pos = case HM.lookup "POSITION" attr of
                Just p' -> mkV3Stream p'
                Nothing -> mempty
        nrm = case HM.lookup "NORMAL" attr of
                Just n' -> mkV3Stream n'
                Nothing -> createNormals pos (VS.unsafeCast idx')
        tex = case HM.lookup "TEXCOORD_0" attr of
                Just t' -> mkV2Stream t'
                Nothing -> VS.map (\(V3 x y _) -> V2 x y) pos
        clr = case HM.lookup "COLOR" attr of
                Just c' -> mkV4Stream c'
                Nothing -> VS.replicate (VS.length pos) (pure 1)

mkV2Stream :: DataStream -> VS.Vector (V2 Float)
mkV2Stream ds = let pl = _dataStreamPayload ds in case (_dataStreamType ds, _dataStreamComponent ds) of
  (Vec2, Float)  -> VS.unsafeCast pl
  (Vec2, UByte)  -> VS.map ((/255.0) . fromIntegral) pl
  (Vec2, UShort) -> VS.map ((/65535.0) . fromIntegral) pl
  (Vec3, _) -> VS.map (\(V3 x y _) -> V2 x y) (mkV3Stream ds)
  (Vec4, _) -> VS.map (\(V4 x y _ _) -> V2 x y) (mkV4Stream ds)
  _ -> mempty

mkV3Stream :: DataStream -> VS.Vector (V3 Float)
mkV3Stream ds = let pl = _dataStreamPayload ds in case (_dataStreamType ds, _dataStreamComponent ds) of
  (Vec3, Float) -> VS.unsafeCast pl
  (Vec3, UByte) -> VS.map ((/255.0) . fromIntegral) pl
  (Vec3, UShort) -> VS.map ((/65535.0) . fromIntegral) pl
  (Vec2, _) -> VS.map (\(V2 x y) -> V3 x y 0) (mkV2Stream ds)
  (Vec4, _) -> VS.map (\(V4 x y z _) -> V3 x y z) (mkV4Stream ds)
  _ -> mempty

mkV4Stream :: DataStream -> VS.Vector (V4 Float)
mkV4Stream ds = let pl = _dataStreamPayload ds in case (_dataStreamType ds, _dataStreamComponent ds) of
  (Vec4, Float) -> VS.unsafeCast pl
  (Vec4, UByte) -> VS.map ((/255.0) . fromIntegral) pl
  (Vec4, UShort) -> VS.map ((/65535.0) . fromIntegral) pl
  _ -> mempty


-- nodes (transforms)

data NodeTree = NodeTree
  { _position :: V3 Float
  , _rotation :: Quaternion Float
  , _scale :: V3 Float
  , _index :: Int
  , _name :: Text
  , _children :: V.Vector NodeTree
  , _mesh :: Maybe Int
  } deriving (Eq, Show)

getName :: No.Node -> Maybe Text
getName (No.Node _cam _cs _skin _matrix _mesh _rot _sc _tr _w name _ext _extra) = name

nodeTree :: Maybe (V.Vector No.Node) -> V.Vector NodeTree
nodeTree Nothing = mempty
nodeTree (Just ns) = V.filter (\tree -> not (Set.member (_index tree) nonRoot)) trees
  where
    untuple (x, y, z) = V3 x y z
    unquat (x, y, z, w) = Quaternion w (V3 x y z)
    (trees, nonRoot) = V.ifoldl' go (mempty, mempty) ns
    appNonRoots rs ns = V.foldl' (\s -> flip Set.insert s . No.unNodeIx) rs ns
    go (ts, rm) ix node = (V.snoc ts (makeTree ix node), maybe rm (appNonRoots rm) (No.children node))
    makeTree ix node = NodeTree
      { _position = maybe 0 untuple (No.translation node)
      , _rotation = maybe (Quaternion 1 0) unquat (No.rotation node)
      , _scale    = maybe 1 untuple (No.scale node)
      , _name = maybe "?UNNAMED?" id (getName node)
      , _index = ix
      , _children = case No.children node of
                      Just cs -> fmap (\i -> let j = No.unNodeIx i in makeTree j (ns V.! j)) cs
                      Nothing -> mempty
      , _mesh = fmap Me.unMeshIx (No.mesh node)
      }




-- things to show a node tree in ghci etc.

data DbgMore = Start | More | End | Singular deriving (Eq, Ord, Show, Enum, Bounded)
indexToMore :: Int -> Int -> DbgMore
indexToMore _ 0 = Singular
indexToMore s n
  | s == n-1 = End
  | s == 0 = Start
  | otherwise = More

debugShowNodeTree :: NodeTree -> String
debugShowNodeTree = go "" Singular
  where
    go pre more node = V.ifoldl (\pre i nd -> pre <> go pre' (indexToMore i n) nd) me (_children node)
      where
        n = V.length (_children node)
        md = case more of
          Start    -> '├' -- '┬'
          More     -> '├'
          End      -> '└'
          Singular -> '─'
        pre' = pre <> case more of
          End -> " "
          Singular -> " "
          _ -> "│"
        me = '\n' : pre <> (md : '*' : ' ' : unpack (_name node))





















--
