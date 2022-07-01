{-# OPTIONS_GHC -Wno-orphans #-}

{-# Language DeriveGeneric, DeriveAnyClass #-}
{-# Language StandaloneDeriving #-}

{- | A compact binary mesh format
-}
module Quern.Codec.SliMesh
  where

import Control.Monad.IO.Class

import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Data.Word

import Data.ByteString as BS

import qualified Data.HashMap.Strict as HM
import Data.Persist
--import Foreign (Storable)
--import Numeric.Half
import Linear

import System.FilePath.Posix

import GHC.Generics (Generic)

import Quern.Render.StaticScene.SceneStorage
import Quern.Orphans()
import Quern.Codec.Obj
import Quern.Codec.Mtl
import Quern.Render.Opacity
{-
p :: Flat a => a -> String
p = prettyShow . bits
-}
{-
deriving instance Flat StaticVertex
deriving instance Flat Opacity
deriving instance Flat a => Flat (Material a)
deriving instance Flat a => Flat (MaterialEx a)
-}
deriving instance Persist StaticVertex
deriving instance Persist Opacity
deriving instance Persist a => Persist (Material a)
deriving instance Persist a => Persist (MaterialEx a)

data SliMeshSlice = SliMeshSlice
  { _sliceMaterial :: !MaterialKey
  , _sliceIndices :: !(V2 Int)
  } deriving (Show, Eq, Ord, Generic, Persist)

currentMeshVersion :: Word32
currentMeshVersion = 0

-- todo: custom decode to handle versioning
-- todo: switch from Vector StaticVertex to StaticVertexBuffer backed by SoA
-- todo: move mesh data into a sub-structure to keep the header info separate?
data SliMesh = SliMesh
  { _meshVersion :: !Word32
  , _meshVertices :: !(VS.Vector StaticVertex)
  , _meshIndices :: !(VS.Vector Word32)
  , _meshSlices :: !(V.Vector SliMeshSlice)
  , _meshMaterials :: !(M.Map MaterialKey (MaterialEx MaterialPathsFallback))
  } deriving (Show, Eq, Ord, Generic, Persist)


sliMeshExt :: String
sliMeshExt = ".qmesh"

fromSliMeshFile :: MonadIO m => FilePath -> m (Maybe SliMesh)
fromSliMeshFile path = liftIO $
  either (const Nothing) Just . decode <$> (BS.readFile path)

toSliMeshFile :: MonadIO m => FilePath -> SliMesh -> m ()
toSliMeshFile path msh = liftIO $
  BS.writeFile path (encode msh)

unObj :: ObjMesh -> HM.HashMap String (MaterialEx MaterialPathsFallback) -> SliMesh
unObj (ObjMesh vs ix mtlAssignments _) mtls = SliMesh currentMeshVersion vs ix slices mtls'
  where
    slices = V.fromList $ fmap mkSlice mtlAssignments
    mkSlice (mtlKey, start, end) = SliMeshSlice mtlKey (V2 start end)
    mtls' = M.fromList (HM.toList mtls)

convertObjFile :: MonadIO m => FilePath -> m (Maybe SliMesh)
convertObjFile path = do
  objE <- liftIO $ parseObjFile path
  case objE of
    Left _ -> pure Nothing
    Right m -> do
      mlib <- parseMtlFile $ replaceFileName path (_objMeshLibrary m)
      let sm = unObj m mlib
      toSliMeshFile (path -<.> sliMeshExt) sm
      pure $ Just sm
