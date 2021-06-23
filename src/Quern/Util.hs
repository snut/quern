module Quern.Util
  (
  -- * Useful number and vector utilities
    quatBetween
  , fract
  , canonicalToUnit
  , smoothstep
  , saturate
  , randomUnits
  , halton
  , gaussian
  , gaussianKernel
  -- * File handle wrapping
  , toFileHandle

  ) where

import Linear
import System.Random

import qualified Data.Vector.Storable as VS

import System.IO as IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)

-- | Quaternion that will rotate the first vector to the second
quatBetween :: V3 Float -> V3 Float -> Quaternion Float
quatBetween u v
  | quadrance q < 1e-4 = Quaternion 0 (V3 0 0 1)
  | otherwise = normalize q
  where
    q = Quaternion w (u `cross` v)
    w = sqrt (quadrance u * quadrance v) + (u `dot` v)

fract :: RealFrac a => a -> a
fract x = abs (x - fromIntegral i)
  where i = floor x :: Int

-- | Create a unit vector based on two uniformly distributed numbers in the range [0,1)
canonicalToUnit :: Float -> Float -> V3 Float
canonicalToUnit u v
  | u < 0 || u > 1 || v < 0 || v > 1 = canonicalToUnit (fract u) (fract v)
  | otherwise = V3 x y z
  where
    z = u * 2 - 1
    u' = sqrt (1 - z*z)
    th = v * 2 * pi
    y = u' * sin th
    x = u' * cos th

{-# INLINEABLE smoothstep #-}
{-# SPECIALIZE smoothstep :: Float -> Float #-}
smoothstep :: (Num a, Ord a) => a -> a
smoothstep x
  | x <= 0 = 0
  | x >= 1 = 1
  | otherwise = 3*x*x - 2*x*x*x

{-# INLINE saturate #-}
{-# SPECIALIZE saturate :: Float -> Float #-}
saturate :: (Num a, Ord a) => a -> a
saturate = min 1 . max 0


randomUnits :: Int -> [V3 Float]
randomUnits seed = zipWith canonicalToUnit (randoms a) (randoms b)
  where
    (a,b) = split (mkStdGen seed)

-- | This is horrible, but it seems *something* is
-- stomping stdout/stderr on windows.
toFileHandle :: FilePath -> Handle -> IO a -> IO a
toFileHandle pth h inner = do
  h' <- hDuplicate h
  openFile pth WriteMode >>= \f -> hDuplicateTo f h
  result <- inner
  hDuplicateTo h' h
  pure result

-- | Halton 2, 3 sequence
halton :: Int -> VS.Vector (V2 Float)
halton count = VS.generate count $ \i -> V2 (haltonIx 2 i) (haltonIx 3 i)

haltonIx :: Int -> Int -> Float
haltonIx b ix = go 1 0 ix
  where
    rcpB = recip (fromIntegral b) :: Float
    go f r i
      | i <= 0 = r
      | otherwise = go f' r' i'
      where
        f' = f * rcpB
        (i', m) = i `divMod` b
        r' = r + f' * (fromIntegral m)

-- | Gaussian function in one dimension.
-- `gaussian sigma x` where sigma is the standard deviation of the distribution
--  kernel width should be ~6 sigma
gaussian :: Float -> Float -> Float
gaussian sigma x = recip (sqrt (pi * s)) * exp y
  where
    s = 2 * sigma * sigma
    y = negate ((x * x) / s)

-- | 1 dimensional gaussian kernel of fixed width
gaussianKernel :: Int -> VS.Vector Float
gaussianKernel n | n > 0 = nrm $ VS.generate n gen
  where
    nrm v = let s = VS.sum v in VS.map (/ s) v
    hlf = fromIntegral (n-1) * 0.5
    sigma = fromIntegral n / 6
    gen i = gaussian sigma $ fromIntegral i - hlf
gaussianKernel _ = mempty
