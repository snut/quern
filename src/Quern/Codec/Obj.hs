{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving, FlexibleInstances #-}

module Quern.Codec.Obj
  ( -- * Load and parse .obj files
    parseObjFile
  , ObjMesh(..)
  , exportObjFile
  ) where


import Quern.Render.StaticScene.SceneStorage
import Linear hiding (el)

import Control.Monad.ST
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
--import           Data.Char (digitToInt)
import Data.Foldable (traverse_, foldlM)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM


import qualified Data.Map as M
import Data.IORef
import Foreign
import Numeric.Half

import Text.Parsec

import System.IO as IO
import Numeric (showFFloat)

import Quern.Codec.Combinators
import Quern.Codec.MeshTypes

--import GHC.Stack





objLine :: ObjBuilder ()
objLine =  (prefixed "vt" vec2 >>= addTC)
       <|> (prefixed "vn" vec3 >>= addNrm)
       <|> (prefixed "v"  vec3 >>= addPos)
       <|> (prefixed "o"  restOfLine >>= addObject)
       <|> (prefixed "f"  face >>= addFace)
       <|> (prefixed "#"  restOfLine *> pure ()) -- comment
       <|> (prefixed "s"  restOfLine *> pure ()) -- smoothing on
       <|> (prefixed "vp" restOfLine *> pure ()) -- parametric vertex
       <|> (prefixed "g"  restOfLine *> pure ()) -- group name
       <|> (prefixed "mg" restOfLine *> pure ()) -- merging group
       <|> (prefixed "mtllib" restOfLine >>= addMtlLib) -- material library file
       <|> (prefixed "usemtl" restOfLine >>= addMtl) -- material name
       <?> "obj-prefixed line"


objParser :: ObjBuilder ObjData
objParser = do
  _ <- endBy objLine end
  (Obj pos nrm tex tri mtls mtlLib) <- getState
  liftIO $ Obj <$> unsafeFreeze pos <*> unsafeFreeze nrm <*> unsafeFreeze tex <*> unsafeFreeze tri <*> pure (reverse mtls) <*> pure mtlLib

--basicObj :: B.ByteString -> ObjBuilder
basicObj :: SourceName -> B.ByteString -> IO (Either ParseError ObjData)
basicObj path bs = do
  ob <- Obj <$> newGrowVec <*> newGrowVec <*> newGrowVec <*> newGrowVec <*> pure [] <*> pure ""
  runParserT objParser ob path bs

data ObjMesh = ObjMesh
  { _objMeshVertices :: !(VS.Vector StaticVertex)
  , _objMeshIndices :: !(VS.Vector Word32)
  , _objMeshMaterials :: ![(String, Int, Int)] -- material name, start of indices, count of indices
  , _objMeshLibrary :: !FilePath -- library
  } deriving (Eq, Show)

countMtls :: [(String, Int)] -> Int -> [(String, Int, Int)]
countMtls [] _ = []
countMtls ms totalIx = go ms (tail ms)
  where
    go [] _ = []
    go ((nm, start):_) [] = [(nm, start, totalIx - start)]
    go ((nm, start):xs) ((_, startNext):ys) = (nm, start, startNext-start) : go xs ys

parseObjFile :: FilePath -> IO (Either String ObjMesh)
parseObjFile path = do

  objE <- B.readFile path >>=  basicObj path -- runParserT objParser ob path
  case objE of
    Right obj -> do
      (vs, tris) <- interleaveVertexData obj
      let staticVerts = convertToStatic vs tris
          staticIndices = VS.unsafeCast tris :: VS.Vector Word32
          mtls = _objMaterials obj
      pure . Right $ ObjMesh staticVerts staticIndices (countMtls mtls (VS.length staticIndices)) (_objMaterialLib obj)
    Left err -> pure $! Left (show err)
--
data ObjF v = Obj
  { _objPositions :: !(v (V3 Float))
  , _objNormals :: !(v (V3 Float))
  , _objTexcoords :: !(v (V2 Float))
  , _objTriangles :: !(v (V3 (V3 Word32)))
  , _objMaterials :: ![(String, Int)]
  , _objMaterialLib :: !FilePath
  }

deriving instance Show (ObjF VS.Vector)

type ObjState = ObjF GrowVec
type ObjData = ObjF VS.Vector
type ObjBuilder a = ParsecT B.ByteString ObjState IO a

{-# INLINE addTC #-}
addTC :: V2 Float -> ObjBuilder ()
addTC x  = getState >>= \ob -> liftIO (pushVec_ (_objTexcoords ob) x)

{-# INLINE addNrm #-}
addNrm :: V3 Float -> ObjBuilder ()
addNrm x = getState >>= \ob -> liftIO (pushVec_ (_objNormals   ob) x)

{-# INLINE addPos #-}
addPos :: V3 Float -> ObjBuilder ()
addPos x = getState >>= \ob -> liftIO (pushVec_ (_objPositions ob) x)

addObject :: String -> ObjBuilder ()
addObject _name = pure ()

addFace :: [V3 Int32] -> ObjBuilder ()
addFace faceIxs = do
  ob <- getState
  numPs <- liftIO $ fromIntegral <$> usedVec (_objPositions ob)
  numNs <- liftIO $ fromIntegral <$> usedVec (_objNormals ob)
  numTs <- liftIO $ fromIntegral <$> usedVec (_objTexcoords ob)
  let tris = triangulate (fixIndices (V3 numPs numTs numNs) <$> faceIxs)
  liftIO $ mapM_ (pushVec_ (_objTriangles ob)) tris

addMtl :: String -> ObjBuilder ()
addMtl mtlName = do
  ob <- getState
  numTris <- liftIO $ usedVec (_objTriangles ob)
  setState $ ob{ _objMaterials = (mtlName, numTris*3) : _objMaterials ob }

addMtlLib :: FilePath -> ObjBuilder ()
addMtlLib mtlLibPath = modifyState $ \ob -> ob{ _objMaterialLib = mtlLibPath }


triangulate :: [a] -> [V3 a]
triangulate [] = []
triangulate (x:xs) = zipWith (V3 x) xs (drop 1 xs)

-- obj indices are 1-based normally but can be negative when relative to the last vertex data emitted
-- [a, b, c, d] !!  1 == a
-- [a, b, c, d] !! -2 == c
-- zero indices seems to be undefined so I'm using them to indicate a missing index
fixIndices :: V3 Int32 -> V3 Int32 -> V3 Word32
fixIndices sz ix = f <$> sz <*> ix
  where
    f s i | i == 0    = 0 -- should never happen
          | i < 0     = fromIntegral $ s + i -- index is relative to end of buffer
          | otherwise = fromIntegral $ i - 1 -- regular 1-based index

-- by default, obj files are left handed and weird
handedness :: Num a => V3 a -> V3 a
handedness (V3 x y z) = V3 x (negate z) y

uvFlip :: V2 Float -> V2 Float
uvFlip (V2 u v) = V2 u (1 - v)

winding :: V3 a -> V3 a
winding = id -- (V3 i j k) = V3 i k j

-- convert independantly indexed pos/tex/nrm things into a compact vertex array
interleaveVertexData :: ObjData -> IO (VS.Vector ObjVertex, VS.Vector (V3 Word32))
interleaveVertexData (Obj ps ns ts tris _mtls _mtlLib) = do
    vs <- newGrowVec
    is <- newGrowVec :: IO (GrowVec Word32)
    let m0 = mempty :: M.Map (V3 Word32) Word32
    _ <- VS.foldM' (go vs is) m0 tris
    vs' <- unsafeFreeze vs
    is' <- unsafeFreeze is
    pure (vs', VS.unsafeCast is')
  where
    go :: GrowVec ObjVertex -> GrowVec Word32 -> M.Map (V3 Word32) Word32 -> V3 (V3 Word32) -> IO (M.Map (V3 Word32) Word32)
    go vs is assoc tri = foldlM (fetchOrAdd vs is) assoc (winding tri)
    fetchOrAdd vs is assoc tripleIx = case M.lookup tripleIx assoc of
      Just ix -> pushVec_ is ix *> pure assoc
      Nothing -> do
        let pos = maybe (V3 0 0 0) handedness $ ps VS.!? ixP -- this should be an error, but some obj files seem to have insufficient positions...
            nrm = maybe (V3 0 0 1) handedness $ ns VS.!? ixN
            tex = maybe 0 id $ ts VS.!? ixT
            vert = ObjVertex ( pos) ( nrm) (uvFlip tex) 0xff
            V3 ixP ixT ixN = (fromIntegral <$> tripleIx) :: V3 Int
        ix <- fromIntegral <$> pushVec vs vert
        pushVec_ is ix
        pure $ M.insert tripleIx ix assoc

-- export

-- pile of tiny builders
bspc, bnewline, bslash :: Builder
bspc = B.char7 ' '
bnewline = B.char7 '\n'
bslash = B.char7 '/'

bfloat :: Float -> Builder
bfloat x = B.string7 (showFFloat (Just 6) x "") -- ick

bvec3 :: V3 Float -> Builder
bvec3 (V3 x y z) = bfloat x <> bspc <> bfloat y <> bspc <> bfloat z <> bnewline

bvec2 :: V2 Float -> Builder
bvec2 (V2 x y) = bfloat x <> bspc <> bfloat y <> bnewline

btriface :: V3 Word32 -> Builder
btriface tri = i <> bspc <> j <> bspc <> k <> bnewline
  where
    V3 i j k = (\d -> d <> bslash <> d <> bslash <> d) . B.word32Dec . succ <$> tri


exportObjFile :: FilePath -> VS.Vector StaticVertex -> VS.Vector Word32 -> IO ()
exportObjFile path vts idx = IO.withFile path IO.WriteMode $ \hdl -> do
    IO.hSetBinaryMode hdl True
    IO.hSetBuffering hdl (BlockBuffering Nothing)
    B.hPutBuilder hdl built
  where
    built = mconcat
      [ B.byteString "# exported from Quern\no Exported\n"
      , positions
      , texcoords
      , normals
      , B.byteString "s off\n"
      , faces ]
    posPrefix = B.char7 'v' <> bspc
    positions = VS.foldl' (\bld vtx -> bld <> posPrefix <> bvec3 (_vertexPos vtx)) mempty vts
    texPrefix = B.string7 "vt" <> bspc
    texcoords = VS.foldl' (\bld vtx -> bld <> texPrefix <> bvec2 (fromHalf <$> _vertexUV0 vtx)) mempty vts
    nrmPrefix = B.string7 "vn" <> bspc
    normals = VS.foldl' (\bld vtx -> bld <> nrmPrefix <> bvec3 (vertexNormal vtx)) mempty vts
    facePrefix = B.char7 'f' <> bspc
    faces = VS.foldl' (\bld t -> bld <> facePrefix <> btriface t) mempty (VS.unsafeCast idx :: VS.Vector (V3 Word32))
