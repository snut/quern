module Main where

import Criterion.Main

import Data.Vector.Storable as VS

import Quern.Codec.DDS.Internal as DDS
import Quern.Codec.Obj as Obj

ddsTest path = either (error "failed to parse DDS") (ddsPayloadSize . _ddsFilePayload) <$> loadDDS path
objTest path = either (error "failed to parse Obj") (\(vs,is) -> VS.length vs + VS.length is) <$> parseObjFile path

main =
  defaultMain
    [ bgroup "dds"
      [ bench "test_bc7" $ nfIO (ddsTest "data/textures/test_bc7.dds")
      ]
      {-
    , bgroup "obj"
      [ bench "cube" $ nfIO (objTest "data/meshes/cube.obj")
      , bench "ball" $ nfIO (objTest "data/meshes/ball.obj")
      ]
      -}
    ]
