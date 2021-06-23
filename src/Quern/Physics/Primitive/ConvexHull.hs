{- | This module is unfinished.

-}

module Quern.Physics.Primitive.ConvexHull where

import Linear

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Quern.Physics.Primitive.Types


data QHFacet = QH
  { _qhIs :: !(V3 Int)
  , _qhNs :: V3 (V.Vector Int)
  } deriving (Eq, Ord, Show, Read)

triToPlane :: V3 Float -> V3 Float -> V3 Float -> V4 Float
triToPlane a b c = V4 nx ny nz (n `dot` a)
  where
    n@(V3 nx ny nz) = normalize ((b-a) `cross` (c-a))

unorderedTriToPlane'WithInterior :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V4 Float
unorderedTriToPlane'WithInterior a b c d = V4 nx ny nz (n `dot` a)
  where
    n0 = normalize ((b-a) `cross` (c-a))
    n@(V3 nx ny nz) = if n0 `dot` d > 0 then negate n0 else n0

facetPlane :: VS.Vector (V3 Float) -> QHFacet -> V4 Float
facetPlane vs qh = triToPlane a b c
  where
    V3 a b c = (vs VS.!) <$> _qhIs qh

facetIs :: V3 Int -> QHFacet -> Bool
facetIs v qh = _qhIs qh == v

facetContainsEdge :: V2 Int -> QHFacet -> Bool
facetContainsEdge (V2 a b) qh
  | a == b = False
  | otherwise = any (== a) v && any (== b) v
  where
    v = _qhIs qh

mkFacets :: Int -> V.Vector QHFacet
mkFacets n = fs
  where
    fs'is = V.zip (V.generate (V.length fs) id) fs
    ff x e = V.map fst $ V.filter (\(_,f) -> not (facetIs x f) && facetContainsEdge e f) fs'is
    fs = V.fromList [ QH (V3 i j k) (ff (V3 i j k) <$> V3 (V2 i j) (V2 j k) (V2 k i))
                    | i <- [0 .. n-1]
                    , j <- [i+1 .. n-1]
                    , k <- [j+1 .. n-1]
                    ]

tetra :: VS.Vector (V3 Float) -> Maybe Shape
tetra ps
  | VS.length ps /= 4 = Nothing
  | quadrance n < 1e-6 || abs (n `dot` (d - a)) < 1e-6 = Nothing -- degenerate
  | otherwise = Just $ Hull ps planes
  where
    n = (b - a) `cross` (c - a)
    a = ps VS.! 0
    b = ps VS.! 1
    c = ps VS.! 2
    d = ps VS.! 3
    planes = VS.fromList
      [ unorderedTriToPlane'WithInterior a b c d
      , unorderedTriToPlane'WithInterior d a b c
      , unorderedTriToPlane'WithInterior c d a b
      , unorderedTriToPlane'WithInterior b c d a]

mkHullFromPoints :: VS.Vector (V3 Float) -> Maybe Shape
mkHullFromPoints ps
  | VS.length ps < 4 = Nothing
  | VS.length ps == 4 = tetra ps
  | otherwise = Nothing -- quickhull ps fs
  where
    n = VS.length ps
    fs = mkFacets n
