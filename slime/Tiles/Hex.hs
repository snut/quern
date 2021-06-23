{-# Language DeriveFunctor #-}

module Tiles.Hex
  (
    Hex(..)
  , neighbours
  , hexToPosition
  , hexTileRegion
  , inHex
  , hexDistance
  -- * Re-export
  , V2(..)
  ) where


--import qualified Data.Foldable as F
import Data.Semigroup (Any(..))
import Linear


{-
Hex tiles are laid out in staggered rows

|02|12|22|
 |01|11|21|
|00|10|20|

-}

data Hex a = Hex !a !a !a !a !a !a
  deriving (Eq, Ord, Show, Read, Functor)

instance Applicative Hex where
  pure a = Hex a a a a a a
  (Hex f0 f1 f2 f3 f4 f5) <*> (Hex x0 x1 x2 x3 x4 x5) =
    Hex (f0 x0) (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

instance Foldable Hex where
  foldMap k (Hex a b c d e f) = k a <> k b <> k c <> k d <> k e <> k f

instance Traversable Hex where
  traverse k (Hex a b c d e f) = Hex <$> k a <*> k b <*> k c <*> k d <*> k e <*> k f

inHex :: Eq a => a -> Hex a -> Bool
inHex a = getAny . foldMap (Any . (== a))

-- Neighbours are ordered anticlockwise starting from +X direction

neighboursEvenY, neighboursOddY :: Hex (V2 Int)
neighboursEvenY = Hex (V2 1 0) (V2 0 1) (V2 (-1) 1) (V2 (-1) 0) (V2 (-1) (-1)) (V2 0 (-1))
neighboursOddY = Hex (V2 1 0) (V2 1 1) (V2 0 1) (V2 (-1) 0) (V2 0 (-1)) (V2 1 (-1))


neighbours :: V2 Int -> Hex (V2 Int)
neighbours p@(V2 _ y)
  | even y = fmap (+ p) neighboursEvenY
  | otherwise = fmap (+ p) neighboursOddY


centerEdge :: Float
centerEdge = cos (pi / 6)

centerCenter :: Float
centerCenter = 2 * centerEdge

rowRow :: Float
rowRow = 1.5 -- == centerEdge * tan (pi / 3)

-- | Assume that the representation of a hex tile is a regular
-- hexagon of radius 1
hexToPosition :: V2 Int -> V2 Float
hexToPosition (V2 x y)
  | even y = V2 x' y'
  | otherwise = V2 (x' + centerEdge) y'
  where
    y' = fromIntegral y * rowRow
    x' = fromIntegral x * centerCenter

-- | Generate hex tiles in a rectangular region
hexTileRegion :: (V2 Int -> V2 Float -> a) -> V2 Int -> V2 Int -> [a]
hexTileRegion f (V2 x0 y0) (V2 x1 y1) = [f (V2 x y) (hexToPosition (V2 x y)) | y <- [y0 .. y1], x <- [x0 .. x1]]

hexDistance :: V2 Int -> V2 Int -> Int
hexDistance a@(V2 ax ay) b@(V2 bx by)
  | a == b = 0
  | ay == by = abs (bx - ax)
  | otherwise = floor $ distance (hexToPosition a) (hexToPosition b) / centerCenter + 0.5
