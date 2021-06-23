{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# Language ScopedTypeVariables #-}
{-# Language BangPatterns #-}

{-| A simple axis-aligned bounding box.

 In one dimension, an interval. In two, a rectangle. In three, an axis-aligned bounding box etc.
-}
module Quern.Physics.AxisAlignedBox
  (
  -- * Construction
    AABox(..)
  , mkAABox
  , mkAABoxPts
  , degenerate
  , expand
  , foldAABox
  , grow
  , quantise
  , translate
  , scaleBox
  -- * Examination
  , volume
  , size
  , center
  , centerFloor
  , aaBoxVertices
  , aaBoxVerticesNE
  , valid
  , clamp
  , aaBoxMin
  , aaBoxMax
  , contained
  -- * Comparison
  , insideOn
  , inside
  , insideInclOn
  , insideIncl
  , aaBoxInBox
  , disjointOn
  , disjoint
  , overlapsOn
  , overlaps
  , intersection
  -- * Newtypes
  , BoundedAABox(..)
  , IntersectAABox(..)
  , FloatAABox(..)
  , floatFoldAABox
  -- * Debugging
  , exportAABoxObj
  ) where

import Control.DeepSeq
import Data.Semigroup
import Linear
import Numeric (showFFloat)
import System.IO (withFile, IOMode(..), hPutStrLn)
import Foreign
import Data.List.NonEmpty (NonEmpty(..))

-- | A simple bounding box.
-- Intended for use with 'V1' .. 'V4' from 'linear', but should work with many finite,
-- zippy-applicative data structures.
-- Most associated functions constrain 'f' to 'Applicative' and 'a' to 'Ord',
-- some also require `f` to be @Foldable@.
data AABox f a = AABox !(f a) !(f a) deriving (Eq, Show, Read, Ord)

-- | Extract the minimal point from the box.
{-# INLINEABLE aaBoxMin #-}
aaBoxMin :: AABox f a -> f a
aaBoxMin (AABox a _) = a

-- | Extract the maximal point from the box.
{-# INLINEABLE aaBoxMax #-}
aaBoxMax :: AABox f a -> f a
aaBoxMax (AABox _ b) = b

instance (NFData (f a)) => NFData (AABox f a) where
  rnf (AABox a b) = rnf a `seq` rnf b `seq` ()

-- | Constructor for boxes.
-- Ensures min and max extents are correctly initialised from two arbitrary points.
mkAABox :: (Ord a, Applicative f) => f a -> f a -> AABox f a
mkAABox !a !b = degenerate a <> degenerate b

-- | Just to encourage use of non-empty
mkAABoxPts :: (Ord a, Applicative f) => NonEmpty (f a) -> AABox f a
mkAABoxPts = sconcat . fmap degenerate

-- | Extents of the box.
size :: (Applicative f, Num a) => AABox f a -> f a
size (AABox a b) = (-) <$> b <*> a

-- | If boxes are constructed partwise, we can invalidate the constraints on min and max.
valid :: (Ord a, Foldable f, Applicative f) => AABox f a -> Bool
valid a = and $ (>=) <$> aaBoxMax a <*> aaBoxMin a

-- | Volume occupied by the box.
volume :: (Foldable f, Applicative f, Num a) => AABox f a -> a
volume = getProduct . foldMap Product . size

-- | Expand the box by a scalar, uniform amount.
-- Edge lengths will increase by twice this amount, as we
-- expand in both positive and negative directions.
--
-- Currently allows shrinking with a negative grow amount.
grow :: (Applicative f, Num a, Ord a) => a -> AABox f a -> AABox f a
grow s (AABox a b) = mkAABox a' b'
  where
    a' = (-) <$> a <*> pure s
    b' = (+) <$> b <*> pure s

-- | Convert a box to an integral numeric type, ensuring it would still contain the original
quantise :: (Functor f, RealFrac a, Integral b) => AABox f a -> AABox f b
quantise (AABox a b) = AABox (floor <$> a) (ceiling <$> b)

-- | All points contained by the box
contained :: (Enum a, Applicative f, Traversable f) => AABox f a -> [f a]
contained (AABox a b) = enumApplicative a b

-- | Constrain a point to lie within a box, with inclusive bounds.
{-# INLINE clamp #-}
clamp :: (Applicative f, Ord a) => f a -> AABox f a -> f a
clamp p (AABox a b) = f <$> a <*> b <*> p
  where f lb ub = min ub . max lb

-- | Vertices of the box
-- NB: generates vertices that vary slowest in first dimension.
-- Somewhat contrary to expectation, that the first component varies fastest,
-- but matches a list comprehension in order.
--
-- >>> boxVertices (AABox 0 1 :: AABox V2 Int)
-- [V2 0 0, V2 0 1, V2 1 0, V2 1 1]
--
-- >>> [V2 x y | x <- [0,1], y <- [0,1]]
-- [V2 0 0, V2 0 1, V2 1 0, V2 1 1]
aaBoxVertices :: (Traversable f, Applicative f) => AABox f a -> [f a]
aaBoxVertices (AABox a b) = sequenceA (pair <$> a <*> b)
  where
    pair x y = [x, y]

aaBoxVerticesNE :: (Traversable f, Applicative f) => AABox f a -> NonEmpty (f a)
aaBoxVerticesNE bx@(AABox a _) =  a :| drop 1 (aaBoxVertices bx)

-- | Faces for creating a mesh from the output of @boxVertices@ (vertex indices, CCW face winding)
aaBoxFaces :: [[Int]]
aaBoxFaces = [[0,2,6,4], [0,4,5,1], [0,1,3,2], [1,5,7,3], [4,6,7,5], [2,3,7,6]]

{-# INLINE degenerate #-}
-- | A box at a location, with size zero.
degenerate :: f a -> AABox f a
degenerate !p = AABox p p


-- | Middle of the box.
center :: (Applicative f, Fractional a) => AABox f a -> f a
center box@(AABox a _) = (+) <$> a <*> ((/2) <$> size box)

-- | Middle of the box, rounding towards the minimum point
centerFloor :: (Applicative f,Integral a) => AABox f a -> f a
centerFloor box@(AABox a _) = (+) <$> a <*> ((`div` 2) <$> size box)

{-# INLINE expand #-}
{-# SPECIALISE expand :: V3 Float -> AABox V3 Float -> AABox V3 Float #-}
-- | Expands a box with a point.
expand :: (Applicative f, Ord a) => f a -> AABox f a -> AABox f a
expand !pt (AABox !a !b) = AABox (min <$> pt <*> a) (max <$> pt <*> b)

-- | The returned value will be True in all dimensions that fall within the interval defined by the box
{-# INLINE insideOn #-}
insideOn :: (Applicative f, Ord a) => f a -> AABox f a -> f Bool
insideOn pt (AABox a b) = interval (>=) (<) <$> pt <*> a <*> b

-- | As 'insideOn' but inclusive of the upper bounds
{-# INLINE insideInclOn #-}
insideInclOn :: (Applicative f, Ord a) => f a -> AABox f a -> f Bool
insideInclOn !pt (AABox !a !b) = interval (>=) (<=) <$> pt <*> a <*> b

{-# INLINEABLE interval #-}
interval :: (a -> b -> Bool) -> (a -> c -> Bool) -> a -> b -> c -> Bool
interval f g a b c = f a b && g a c

{-# INLINE inside #-}
-- | @p `inside` b@ indicates that the point 'p' is contained within box 'b'
inside :: (Applicative f, Foldable f, Ord a) => f a -> AABox f a -> Bool
inside pt box = and $! insideOn pt box
infix 4 `inside`

{-# INLINE insideIncl #-}
-- | As @inside@ but this test is inclusive of upper bounds.
insideIncl :: (Applicative f, Foldable f, Ord a) => f a -> AABox f a -> Bool
insideIncl !pt !box = and $! insideInclOn pt box
infix 4 `insideIncl`

-- | A box `inner` is inside another box `outer` if its min and max extents are `insideIncl` of `outer`.
aaBoxInBox :: (Applicative f, Foldable f, Ord a) => AABox f a -> AABox f a -> Bool
aaBoxInBox (AABox a b) box = a `insideIncl` box &&  b `insideIncl` box

-- | Create a box from a `Foldable` of points. Empty structures return `Nothing`.
-- Singleton structures will return a degenerate box.
foldAABox :: (Applicative f, Foldable t, Ord b) => (a -> f b) -> t a -> Maybe (AABox f b)
foldAABox f = getOption . foldMap (Option . Just . degenerate . f)

-- | The resulting functor will have 'True' for each axis the two boxes are disjoint on.
disjointOn :: (Ord a, Applicative t) => AABox t a -> AABox t a -> t Bool
disjointOn (AABox minA maxA) (AABox minB maxB) = (||) <$> gt <*> lt
    where
      gt = (>) <$> minA <*> maxB
      lt = (<) <$> maxA <*> minB

-- | Opposite of 'disjointOn'. Axes containing 'True' indicate overlap of the two boxes along that axis.
overlapsOn :: (Ord a, Applicative t) => AABox t a -> AABox t a -> t Bool
overlapsOn = (fmap not .) . disjointOn

-- | If any axis is disjoint, the boxes as a whole are.
disjoint :: (Ord a, Applicative t, Foldable t) => AABox t a -> AABox t a -> Bool
disjoint a b = or (disjointOn a b)

-- | If all axes overlap, the boxes overlap.
-- This is an inclusive test: boxes that abutt without intersection count as overlapping.
overlaps :: (Ord a, Applicative t, Foldable t) => AABox t a -> AABox t a -> Bool
overlaps a b = not (disjoint a b)

-- | Intersection of boxes.
-- Gives Nothing on disjoint boxes.
-- Note that this may be degenerate in one or more axes if the boxes are touching.
intersection :: (Ord a, Applicative f, Foldable f) => AABox f a -> AABox f a -> Maybe (AABox f a)
intersection (AABox minA maxA) (AABox minB maxB)
  | valid i   = Just i
  | otherwise = Nothing
  where
    i = AABox (max <$> minA <*> minB) (min <$> maxA <*> maxB)

-- | Translate a box
translate :: (Num a, Applicative f) => f a -> AABox f a -> AABox f a
translate t (AABox a b) = AABox ((+) <$> a <*> t) ((+) <$> b <*> t)

-- | Ssale a box
scaleBox :: (Num a, Applicative f) => f a -> AABox f a -> AABox f a
scaleBox s (AABox a b) = AABox ((*) <$> s <*> a) ((+) <$> s <*> b)

-- * Instances
instance Functor f => Functor (AABox f) where
  {-# INLINE fmap #-}
  fmap f (AABox a b) = AABox (fmap f a) (fmap f b)

instance Applicative f => Applicative (AABox f) where
  {-# INLINE pure #-}
  pure x = AABox (pure x) (pure x)
  {-# INLINE (<*>) #-}
  (AABox f g) <*> (AABox x y) = AABox (f <*> x) (g <*> y)

instance Monad m => Monad (AABox m) where
  (AABox x y) >>= k = AABox (x >>= aaBoxMin . k) (y >>= aaBoxMax . k)
  {-# INLINE return #-}
  return = pure

-- | The semigroup operation for 'AABox'es joins them to form ever larger boxes.
instance (Applicative f, Ord a) => Semigroup (AABox f a) where
  {-# INLINE (<>) #-}
  (AABox a b) <> (AABox c d) = AABox (min <$> a <*> c) (max <$> b <*> d)


instance Foldable f => Foldable (AABox f) where
  foldMap f (AABox a b) = foldMap f a `mappend` foldMap f b

instance Traversable f => Traversable (AABox f) where
  traverse f (AABox a b) = AABox <$> traverse f a <*> traverse f b

instance Storable (f a) => Storable (AABox f a) where
  peek p = AABox <$> peek (castPtr p) <*> peek (plusPtr p (sizeOf (undefined:: f a)))
  poke p (AABox a b) = poke (castPtr p) a *> poke (plusPtr p (sizeOf a)) b
  sizeOf _ = sizeOf (undefined :: f a) * 2
  alignment _ = alignment (undefined :: f a)

-- | Newtype for use with 'Bounded' element types.
-- 'mempty' will produce a box with flipped extents.
-- This is only a newtype because the instance makes working with bounded types
-- more awkward sometimes, 'Option' from 'Semigroup' may be a nicer fit.
newtype BoundedAABox f a = BoundedAABox
  { getBoundedAABox :: AABox f a
  } deriving (Eq, Ord, Show, Read, Functor, Applicative, Monad, Semigroup)

instance (Bounded a, Applicative f, Ord a) => Monoid (BoundedAABox f a) where
  mempty = BoundedAABox (AABox (pure maxBound) (pure minBound))
  mappend = (<>)

instance (Foldable f) => Foldable (BoundedAABox f) where
  foldMap f (BoundedAABox box) = foldMap f box

instance (Traversable f) => Traversable (BoundedAABox f) where
  traverse f (BoundedAABox box) = BoundedAABox <$> traverse f box

-- | This newtype allows semigroup composition of box intersection.
newtype IntersectAABox f a = IntersectAABox
  { getIntersectAABox :: Maybe (AABox f a)
  } deriving (Eq, Ord, Show, Read, Functor)

instance (Ord a, Num a, Applicative f, Foldable f) => Semigroup (IntersectAABox f a) where
  (IntersectAABox (Just a)) <> (IntersectAABox (Just b)) = IntersectAABox (intersection a b)
  _ <> _ = IntersectAABox Nothing

instance (Ord a, Num a, Bounded a, Applicative f, Foldable f) => Monoid (IntersectAABox f a) where
  mempty = IntersectAABox (Just (AABox (pure minBound) (pure maxBound)))
  mappend = (<>)

instance (Applicative f) => Applicative (IntersectAABox f) where
  (IntersectAABox (Just boxf)) <*> (IntersectAABox (Just boxa)) = IntersectAABox (Just (boxf <*> boxa))
  _ <*> _ = IntersectAABox Nothing
  pure = IntersectAABox . Just . pure

instance (Foldable f) => Foldable (IntersectAABox f) where
  foldMap f (IntersectAABox (Just box)) = foldMap f box
  foldMap _ _ = mempty


instance (Traversable f) => Traversable (IntersectAABox f) where
  traverse f (IntersectAABox (Just box)) = IntersectAABox . Just <$> traverse f box
  traverse _ _ = pure (IntersectAABox Nothing)


-- | Treat boxes of floating values as monoidal, with mempty having a min of Infinity and max of -Infinity.
newtype FloatAABox f a = FloatAABox
  { getFloatAABox :: AABox f a
  } deriving (Eq, Ord, Show, Read, Functor, Applicative, Monad, Semigroup)

instance (Applicative f, Floating a, Ord a) => Monoid (FloatAABox f a) where
  mempty = FloatAABox $ AABox (pure (1/0)) (pure ((-1)/0))
  mappend = (<>)

-- | Produce a bounding box from a @Foldable@ containing @Applicatives@ (over @Floating@ values)
-- If the container is empty the box will be infinite in size and inverted
floatFoldAABox :: (Foldable t, Applicative f, Ord a, Floating a) => (b -> f a) -> t b -> AABox f a
floatFoldAABox f = getFloatAABox . foldMap (FloatAABox . degenerate . f)

-- | Export to an .obj file for debugging
exportAABoxObj :: FilePath -> AABox V3 Float -> IO ()
exportAABoxObj path box = withFile path WriteMode $ \handle -> do
    hPutStrLn handle $ "# " ++ show box
    hPutStrLn handle $ "o AABox"
    hPutStrLn handle $ "s off"
    mapM_ (hPutStrLn handle . asV) (aaBoxVertices box)
    mapM_ (hPutStrLn handle . asF) aaBoxFaces
  where
    asV = foldl (\ln c -> ln ++ ' ' : showF c) "v"
    showF x = showFFloat (Just 5) x ""
    asF :: [Int] -> String
    asF = foldl (\ln i -> ln ++ ' ' : show (succ i)) "f"



-- | Enumerate the range between two applicatives, in all positions
enumApplicative :: (Enum a, Applicative f, Traversable f) => f a -> f a -> [f a]
enumApplicative a b = sequenceA (enumFromTo <$> a <*> b)
