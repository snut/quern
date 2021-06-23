{-# Language DeriveGeneric #-}
{-# Language TypeApplications #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleInstances #-}
module Quern.Codec.FBX
  ( FBXMesh(..)
  , fromFBXMeshFile
  ) where

import Control.Monad (when)
import Control.Monad.ST
import qualified Control.Monad.State as St
import Data.Bits
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Persist
import qualified Data.Persist.Internal as PI
import Data.Int
import Data.Word
import Foreign
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Codec.Compression.Zlib

import GHC.Generics (Generic)
--import Debug.Trace (traceM)
--import Text.PrettyPrint.GenericPretty

import qualified Data.Char as C8
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

import Linear
import Quern.Render.StaticScene.SceneStorage (StaticVertex(..), MeshKey)
import Quern.Codec.MeshTypes

{-
https://code.blender.org/2013/08/fbx-binary-file-format-specification/

Since 2016 (version 7.5), FBX files encode some things (offsets?) as 64-bit values

http://help.autodesk.com/view/FBX/2017/ENU/?guid=__files_GUID_2ADB9BCE_15EA_485F_87C2_32D43F2D219D_htm

Updated the FBX file format to 7.5. This new file format is only compatible with the 2016 or later product versions that use 64-bit integers in the binary block headers for large file support (> 2GB). The new file format cannot be imported in 2015 or older product versions. You must set the correct export file format for an FBX file, if it needs to be opened in the 2015 or older product versions.
-}

-- | File magic: string
fbxMagic :: B8.ByteString
fbxMagic = B8.pack "Kaydara FBX Binary  "
-- | File magic 2: null terminator, two bytes of unknown purpose
fbxMagic2 :: BS.ByteString
fbxMagic2 = BS.pack [0x00, 0x1A, 0x00]

data FBXFile = FBXFile
  { _fbxFileHeader :: FBXHeader
  , _fbxFileTopLevel :: Vector FBXNode
  } deriving (Show, Generic)

instance Persist FBXFile where
  put _ = error "FBX file writing is not supported"
  get = do
    end <- getEnd
    --traceM $ "FBX End: " <> show end
    hdr <- get
    if _fbxVersion hdr < 7500
      then FBXFile hdr . V.fromList <$> getChildren (getNode32 0) end
      else FBXFile hdr . V.fromList <$> getChildren (getNode64 0) end

data FBXHeader = FBXHeader
  { _fbxVersion :: !Word32
  } deriving (Eq, Ord, Show, Generic)

instance Persist FBXHeader where
  put (FBXHeader ver) = do
    put fbxMagic
    put fbxMagic2
    put ver
  get = do
    m1 <- getBytes 20
    m2 <- getBytes 3
    when (m1 /= fbxMagic || m2 /= fbxMagic2) $
      fail $ unlines [ "FBX File-magic invalid, got:"
                     , B8.unpack m1 <> " | " <> show (BS.unpack m2)
                     , "  need:"
                     , B8.unpack fbxMagic <> " | " <> show (BS.unpack fbxMagic2)]
    v <- get
    --traceM ("FBX Version: " <> show v)
    pure $ FBXHeader v

data FBXNode = FBXNode
  { _fbxNodeName :: !B8.ByteString
  , _fbxNodeLevel :: !Int -- purely for debugging
  , _fbxNodeProperties :: !(Vector FBXProperty)
  , _fbxNodeChildren :: !(Vector FBXNode)
  } deriving (Show, Generic)

getOffset :: Get Int
getOffset = PI.getOffset

getEnd :: Get Int
getEnd = PI.Get $ \e p -> pure $! p PI.:!: (PI.geEnd e `minusPtr` PI.geBegin e)

-- | Sequences of nodes end in an empty record (no properties or name)
getChildren :: Get FBXNode -> Int -> Get [FBXNode]
getChildren node end = do
  offset <- getOffset
  if offset >= end
    then pure []
    else do
      n <- node
      if null (_fbxNodeProperties n) && B8.null (_fbxNodeName n)
        then pure [n]
        else (n:) <$> getChildren node end

-- Unsure which of these values are 64 bit, presumably the end offset at least
getNode64 :: Int -> Get FBXNode
getNode64 lvl = do
  end <- get :: Get Word64
  propCount <- get :: Get Word32
  _propBytes <- get :: Get Word64
  nameCount <- get :: Get Word8
  name <- getBytes (fromIntegral nameCount)
  props <- V.replicateM (fromIntegral propCount) get
  children <- V.fromList <$> getChildren (getNode64 (succ lvl)) (fromIntegral end)
  pure $ FBXNode name lvl props children

-- | Old format uses smaller integers
getNode32 :: Int -> Get FBXNode
getNode32 lvl = do
  end <- get :: Get Word32
  propCount <- get :: Get Word32
  _propBytes <- get :: Get Word32
  nameCount <- get :: Get Word8
  name <- getByteString (fromIntegral nameCount)
  props <- V.replicateM (fromIntegral propCount) get
  children <- V.fromList <$> getChildren (getNode32 (succ lvl)) (fromIntegral end)
  pure $ FBXNode name lvl props children

-- Blender exports version 7.3 by default, as of Blender 2.81
instance Persist FBXNode where
  put _ = error "FBX node writing is not supported"
  get = getNode32 0

unchar :: Word8 -> Char
unchar = C8.chr . fromIntegral

{-
Property tags (char8):
'Y' - Int16
'C' - Bool encoded as LSB of Word8
'I' - Int32
'F' - Float
'D' - Double
'L' - Int64

scalar types are immediately after the tag

'f' - Array Float
'd' - Array Double
'l' - Array Int64
'i' - Array Int32
'b' - Array Bool (Word8: 0, 1)

array types have a header:
  len :: Word32 - number of entries
  encoding :: Word32 - if 0, data is just simple. if 1, deflate/zip compressed array of compressedLen bytes
  compressedLen :: Word32

'S' - string
'R' - raw

both have a Word32 header indicating length in bytes
strings are ASCII, not null-terminated and may contains null bytes
-}

data FBXProperty
  = FBX_I16 !Int16
  | FBX_I32 !Int32
  | FBX_I64 !Int64
  | FBX_Bool !Bool
  | FBX_F32 !Float
  | FBX_F64 !Double
  | FBX_AI32 !(VS.Vector Int32)
  | FBX_AI64 !(VS.Vector Int64)
  | FBX_AF32 !(VS.Vector Float)
  | FBX_AF64 !(VS.Vector Double)
  | FBX_ABool !(VS.Vector Word8)
  | FBX_String !B8.ByteString
  | FBX_Raw !BS.ByteString
  deriving (Generic)

getBool8 :: Get Bool
getBool8 = do
  b <- get :: Get Word8
  if b == 0
    then pure True
    else pure False

getArr :: (Persist a, Storable a) => Get (VS.Vector a)
getArr = do
  len <- get :: Get Word32
  enc <- get :: Get Word32
  clen <- get :: Get Word32
  if enc == 0
    then VS.replicateM (fromIntegral len) get
    else do
      bs <- getByteString (fromIntegral clen)
      case decompress (BL.fromStrict bs) of
        Left err -> fail ("FBXProperty decompression failed: " <> show err)
        Right bs' -> do
          case runGet (VS.replicateM (fromIntegral len) get) (BL.toStrict bs') of
            Left err -> fail ("FBXProperty failed after decompression: " <> err)
            Right v -> pure v

instance Persist FBXProperty where
  put _ = error "FBXProperty writing is not supported"
  get = do
    b <- unchar <$> get
    case b of
      'Y' -> FBX_I16 <$> get
      'I' -> FBX_I32 <$> get
      'L' -> FBX_I64 <$> get
      'C' -> FBX_Bool <$> getBool8
      'F' -> FBX_F32 <$> get
      'D' -> FBX_F64 <$> get
      'i' -> FBX_AI32 <$> getArr
      'l' -> FBX_AI64 <$> getArr
      'f' -> FBX_AF32 <$> getArr
      'd' -> FBX_AF64 <$> getArr
      'b' -> FBX_ABool <$> getArr
      'S' -> do
        l <- get :: Get Word32
        FBX_String <$> getByteString (fromIntegral l)
      'R' -> do
        l <- get :: Get Word32
        FBX_Raw <$> getBytes (fromIntegral l)
      _ -> fail ("Unknown property tag: " <> show b)

-- A custom non-rule-abiding show that just displays the length of array properties
instance Show FBXProperty where
  show (FBX_I16 i) = show i
  show (FBX_I32 i) = show i
  show (FBX_I64 i) = show i
  show (FBX_Bool b) = show b
  show (FBX_F32 f) = show f
  show (FBX_F64 d) = show d
  show (FBX_AI32 v) = "I32*" <> show (VS.length v)
  show (FBX_AI64 v) = "I64*" <> show (VS.length v)
  show (FBX_AF32 v) = "F32*" <> show (VS.length v)
  show (FBX_AF64 v) = "F64*" <> show (VS.length v)
  show (FBX_ABool v) = "B8*" <> show (VS.length v)
  show (FBX_String bs) = show bs
  show (FBX_Raw bs) = "RAW*" <> show (BS.length bs)

-- some enums that appear as strings
data MappingInformationType
  = ByPolygonVertex
  | AllSame
  deriving (Eq, Ord, Enum, Show, Read, Bounded)

data ReferenceInformationType
  = Direct
  | IndexToDirect
  deriving (Eq, Ord, Enum, Show, Read, Bounded)


-- eventual result:
-- Vector StaticVertex
-- list of (MeshKey, Vector Word32)
data FBXMesh = FBXMesh
  { _fbxMeshVertices :: !(VS.Vector StaticVertex)
  , _fbxMeshIndices :: !(VS.Vector Word32)
  , _fbxMeshName :: !String
  } deriving (Eq, Show)

data FBXIntermediate f = FBXIntermediate
  { _iVertices :: !(VS.Vector (V3 Float))
  , _iRawIndices :: !(VS.Vector Int32)
  --, _iIndices :: !(VS.Vector (V3 Word32))
  , _iNormals :: !(VS.Vector (V3 Float))
  , _iNormalIx :: f (VS.Vector Int)
  , _iUVs :: !(VS.Vector (V2 Float))
  , _iUVIx :: f (VS.Vector Int)
  , _iColours :: !(VS.Vector (V4 Float))
  , _iColourIx :: f (VS.Vector Int)
  }

deriving instance Show (FBXIntermediate Maybe)
deriving instance Show (FBXIntermediate Identity)

loadFBXFile :: St.MonadIO m => FilePath -> m (Either String FBXFile)
loadFBXFile fp = St.liftIO (BS.readFile fp) >>= pure . decode

fromFBXMeshFile :: St.MonadIO m => FilePath -> m (Either String FBXMesh)
fromFBXMeshFile fp = do
  fb <- loadFBXFile fp
  case fb of
    Left err -> pure (Left err)
    Right fbx -> do
      case extractMeshes fbx of
        ((name, inter):_) -> do
          (vs,is) <- interleaveFBX inter
          let vs' = convertToStatic vs is
          pure . Right $ FBXMesh vs' (VS.unsafeCast is) (B8.unpack (B8.takeWhile (/= '\NUL') name))
        [] -> pure (Left $ "No meshes loaded from: " <> fp)

-- vertex identified by four indices:
-- one each into the position, normal, uv, colour arrays
type VtxID = V4 Int

data FBXBuilder = FBXBuilder
  { _buildMap :: !(HM.HashMap VtxID Word32)
  , _buildNextIdx :: !Word32
  }

addOrIndex :: St.MonadState FBXBuilder m => VtxID -> m (Word32, Bool)
addOrIndex vtx = do
  FBXBuilder m nxt <- St.get
  case HM.lookup vtx m of
    Just w -> pure (w, False)
    Nothing -> do
      St.put $! FBXBuilder (HM.insert vtx nxt m) (succ nxt)
      pure (nxt, True)

inV :: Storable a => Identity (VS.Vector a) -> Int -> a
inV v ix = runIdentity $ fmap (VS.! ix) v

infixl 8 `inV`


{-
polygons are encoded as sequences of indices terminated by a negative index
the negative index can be converted to a valid index by  bitwise complement
(or negating and subtracting 1)

triangles are more or less trivial, but arbitrary polygons must be triangulated
for this a simple fan is generated, no attempt is made to handle convex n-gons
-}
addNthVertex :: (St.MonadState FBXBuilder m, St.MonadIO m) => Int -> GrowVec ObjVertex -> FBXIntermediate Identity -> m (Word32, Bool)
addNthVertex i vertices inter = do
  let pos = _iRawIndices inter VS.! i
      endPoly = pos < 0
      pos' = if endPoly then complement pos else pos
      vtxId@(V4 posIx nrmIx uvIx clrIx) = V4 (fromIntegral pos') (_iNormalIx inter `inV` i) (_iUVIx inter `inV` i) (_iColourIx inter `inV` i)
  (ix, needAdd) <- addOrIndex vtxId
  --pushVec indices ix
  when needAdd $ do
    let vtx = ObjVertex (_iVertices inter VS.! posIx) (_iNormals inter VS.! nrmIx) (_iUVs inter VS.! uvIx) (_iColours inter VS.! clrIx)
    St.liftIO $ pushVec_ vertices vtx
  pure (ix, endPoly)

completeTriangle :: (St.MonadState FBXBuilder m, St.MonadIO m) => Int -> Word32 -> Word32 -> GrowVec ObjVertex -> GrowVec (V3 Word32) -> FBXIntermediate Identity -> m ()
completeTriangle i a b vertices indices inter = do
  (c, end) <- addNthVertex i vertices inter
  St.liftIO $ pushVec_ indices (V3 a b c)
  let i' = succ i
  if end
    then addTriangle i' vertices indices inter
    else completeTriangle i' a c vertices indices inter

addTriangle :: (St.MonadState FBXBuilder m, St.MonadIO m) => Int -> GrowVec ObjVertex -> GrowVec (V3 Word32) -> FBXIntermediate Identity -> m ()
addTriangle i vertices indices inter
  | VS.length (_iRawIndices inter) < (i+3) = pure ()
  | otherwise = do
    (a,_) <- addNthVertex (i+0) vertices inter
    (b,_) <- addNthVertex (i+1) vertices inter
    completeTriangle (i+2) a b vertices indices inter

addAllVerts :: (St.MonadState FBXBuilder m, St.MonadIO m) => GrowVec ObjVertex -> GrowVec (V3 Word32) -> FBXIntermediate Identity -> m ()
addAllVerts = addTriangle 0

buildFBX :: (St.MonadIO m) => FBXIntermediate Identity -> m (VS.Vector ObjVertex, VS.Vector (V3 Word32))
buildFBX inter = St.liftIO $ do
  vs <- newGrowVec
  is <- newGrowVec
  St.evalStateT (addAllVerts vs is inter) (FBXBuilder mempty 0)
  vs' <- unsafeFreeze vs
  is' <- unsafeFreeze is
  pure (vs', is')

interleaveFBX :: St.MonadIO m => FBXIntermediate Maybe -> m (VS.Vector ObjVertex, VS.Vector (V3 Word32))
interleaveFBX = buildFBX . normaliseFBX

normaliseFBX :: FBXIntermediate Maybe -> FBXIntermediate Identity
normaliseFBX inter = FBXIntermediate
    { _iVertices = _iVertices inter
    , _iRawIndices = _iRawIndices inter
    , _iNormals = ns
    , _iNormalIx = Identity ns'i
    , _iUVs = uvs
    , _iUVIx = Identity uvs'i
    , _iColours = cs
    , _iColourIx = Identity cs'i
    }
  where
    (ns, ns'i) = case _iNormalIx inter of
      Just i -> (_iNormals inter, i)
      Nothing -> directToIndexed (_iNormals inter)
    (uvs, uvs'i) = case _iUVIx inter of
      Just i -> (_iUVs inter, i)
      Nothing -> directToIndexed (_iUVs inter)
    (cs, cs'i) = case _iColourIx inter of
      Just i -> (_iColours inter, i)
      Nothing -> directToIndexed (_iColours inter)


{- normals/uvs/colour attributes

an FBX mesh has P positions and V >= P vertices

these additional attributes have a mapping type and a reference type

observed 'mapping' types:
- ByPolygonVertex
- AllSame

observed reference types:
- Direct (one attribute/vertex)
- IndexToDirect (to vertex)

The most common reference type is IndexToDirect
In this case there are A attribute values, and V indices
The indices reference positions in the attribute array

to ensure good sharing, when adding a rich vertex to our final mesh
we can first check if we have a matching indexed vertex

Direct references can (should?) be converted to indexed ones
-}

directToIndexed :: (Hashable a, Eq a, Storable a) => VS.Vector a -> (VS.Vector a, VS.Vector Int)
directToIndexed as
  | VS.null as = (mempty, mempty)
  | otherwise = runST $ do
    ixs <- VSM.new (VS.length as)
    -- create a mapping from attribute to index, and fill index vector
    (ixMap, _, _) <- VS.foldM' (\(m,i,k) a -> let (m',j,i') = ixA m i a in VSM.unsafeWrite ixs k j *> pure (m', i', succ k)) (mempty, 0, 0) as
    -- create attribute vector
    vals <- VSM.new (length ixMap)
    _ <- HM.traverseWithKey (\a i -> VSM.unsafeWrite vals i a *> pure ixMap) ixMap
    (,) <$> VS.unsafeFreeze vals <*> VS.unsafeFreeze ixs
  where
    ixA m i a = case HM.lookup a m of
      Just j -> (m, j, i)
      Nothing -> (HM.insert a i m, i, succ i)


-- digging through nested nodes to find relevant data

fbxObjects :: B8.ByteString
fbxObjects = B8.pack "Objects"

fbxGeometry :: B8.ByteString
fbxGeometry = B8.pack "Geometry"

extractMeshes :: FBXFile -> [(B8.ByteString, FBXIntermediate Maybe)]
extractMeshes fbx = case findObjects fbx of
  Just obs -> extractFromObjects obs
  Nothing -> []

findObjects :: FBXFile -> Maybe FBXNode
findObjects = V.find (\n -> _fbxNodeName n == fbxObjects) . _fbxFileTopLevel


findChild :: B8.ByteString -> FBXNode -> Maybe FBXNode
findChild named = V.find (\n -> _fbxNodeName n == named) . _fbxNodeChildren

findNameProp :: Vector FBXProperty -> Maybe B8.ByteString
findNameProp v
  | null v = Nothing
  | otherwise = case V.head v of
    FBX_String s -> Just s
    _ -> findNameProp (V.tail v)

extractFromObjects :: FBXNode -> [(B8.ByteString, FBXIntermediate Maybe)]
extractFromObjects obs = case findChild fbxGeometry obs of
  Nothing -> []
  Just geo -> case extractFromGeometry geo of
    Nothing -> []
    Just x -> [x]

extractFromGeometry :: FBXNode -> Maybe (B8.ByteString, FBXIntermediate Maybe)
extractFromGeometry geo = (,) <$> n <*> inter
  where
    n = findNameProp (_fbxNodeProperties geo)
    inter = FBXIntermediate
            <$> (vs >>= getV3Array . _fbxNodeProperties)
            <*> raw
            <*> (ns >>= findChild "Normals" >>= getV3Array . _fbxNodeProperties)
            <*> pure (ns >>= findChild "NormalIndex" >>= getIntArray . _fbxNodeProperties)
            <*> pure (maybe (VS.replicate numIx 0) id (uv >>= findChild "UV" >>= getV2Array . _fbxNodeProperties))
            <*> pure (uv >>= findChild "UVIndex" >>= getIntArray . _fbxNodeProperties)
            <*> pure (maybe (VS.replicate numIx 1) id (cs >>= findChild "Colors" >>= getV4Array . _fbxNodeProperties))
            <*> pure (cs >>= findChild "ColorIndex" >>= getIntArray . _fbxNodeProperties)
    vs = findChild "Vertices" geo
    ns = findChild "LayerElementNormal" geo
    cs = findChild "LayerElementColor" geo
    uv = findChild "LayerElementUV" geo
    is = findChild "PolygonVertexIndex" geo
    raw = is >>= getIndices . _fbxNodeProperties
    numIx = maybe 1 VS.length raw :: Int

getFloatArray :: Vector FBXProperty -> Maybe (VS.Vector Float)
getFloatArray v
  | null v = Nothing
  | otherwise = case V.head v of
    FBX_AF32 fs -> Just $! fs
    FBX_AF64 ds -> Just $! VS.map realToFrac ds
    _ -> getFloatArray (V.tail v)

getIntArray :: Vector FBXProperty -> Maybe (VS.Vector Int)
getIntArray v
  | null v = Nothing
  | otherwise = case V.head v of
    FBX_AI32 is -> Just $! VS.map fromIntegral is
    FBX_AI64 ls -> Just $! VS.map fromIntegral ls
    _ -> getIntArray (V.tail v)

getV2Array :: Vector FBXProperty -> Maybe (VS.Vector (V2 Float))
getV2Array = fmap VS.unsafeCast . getFloatArray

getV3Array :: Vector FBXProperty -> Maybe (VS.Vector (V3 Float))
getV3Array = fmap VS.unsafeCast . getFloatArray

getV4Array :: Vector FBXProperty -> Maybe (VS.Vector (V4 Float))
getV4Array = fmap VS.unsafeCast . getFloatArray

getIndices :: Vector FBXProperty -> Maybe (VS.Vector Int32)
getIndices v
  | null v = Nothing
  | otherwise = case V.head v of
    FBX_AI32 is -> Just $! is
    FBX_AI64 ls -> Just $! VS.map fromIntegral ls
    _ -> getIndices (V.tail v)


{-
notes on decoded test cube, exported from Blender with defaults:
there is a level0 node named "Objects" with no properties
this has a child node:
 "Geometry", properties some kind of int64 id? 817760361, a string "Cube\NUL\SOHGeometry"
  a string "Mesh"
 this had many children including
  "Vertices" with an array of doubles
  "PolygonVertexIndex" with an array of int32 (indices, negative indicate end-of-polygon: negate and subtract 1 to get this idx)
  "Edges" with an array of int32 (refer to the PolygonVertexIndex, each edge is thi vert then the next in the polygon)
  "LayerElementNormal" with an int32 property 0, and bunch of children including
    "MappingInformationType" with a property "ByPolygonVertex"
    "ReferenceInformationType" with a property "Direct"
    "Normals" with an array of doubles (looked like triples)
  "LayerElementColor" with an int32 property 0 and children including
    "MappingInformationType" prop "ByPolygonVertex"
    "ReferenceInformationType" prop "IndexToDirect"
    "Colors" with an array of doubles (RGBA or BGRA)
    "ColorIndex" with an array of int32
  "LayerElementUV"
    "MappingInformationType" prop "ByPolygonVertex"
    "ReferenceInformationType" prop "IndexToDirect"
    "UV" with an array of doubles (pairs)
    "UVIndex" with an array of int32
  "LayerElementMaterial"
    "MappingInformationType"  prop "AllSame"
    "ReferenceInformationType" prop "IndexToDirect"
    "Materials" with an array of int32 containing a single value 0
  "Layer"
    "LayerElement"
      "Type" "LayerElementNormal"
      "TypedIndex" 0
    "LayerElement"
      "Type" "LayerElementColor"
      etc
  "Model" (back on level 1)
    Props have an id 57107590 and the same strings as geometry
    Bunch of nested nodes containing transforms?
  "Material" (id, strings)
    "Properties70" (??)
      "P" ["DiffuseColor", "Color", "", "A"??, R, G, B (float triple)]
      "P" ["AmbientColor", ...]

"Connections" (level 0)
  "C" ["OO", ID, ID]
  "C" ["OO", ID, ID]
  ..
-}
