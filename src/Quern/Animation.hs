{-# Language GADTs #-}
{-# Language RankNTypes #-}

module Quern.Animation
  (
  -- * Curves
    Curve(..)
  -- * Combinators and basic curve types
  , constCurve
  , idCurve
  , smooth
  , backwards
  , bounce
  , curveIn
  , crop
  , easeIn
  , easeOut
  -- * Specific curves
  , tween
  , tweenMultiplier
  , tweenRotation
  , tweenInstance
  , parabola
  , squish
  , roll
  -- * Classes for things that have a duration in time
  , HasBeginTime(..)
  , HasEndTime(..)
  , HasDuration(..)
  -- * Newtypes for disambiguating Doubles
  , Begin(..)
  , Duration(..)
  -- * Clips
  , Clip(..)
  , mkClip
  , mkClip01
  , clipEval
  , clipIn
  -- * Animations are groups of clips
  , Animation(..)
  , mkAnimation
  , mkAnimation01
  , animationEval
  , animationEmpty
  , animationIn
  , idAnimation
  , maybeAnimation
  , pruneAnimation
  , after
  , sequentially
  -- * Re-exported lens types
  , Lens'
  , Traversal'
  ) where

import Control.Lens hiding (backwards)
import Data.Foldable (foldl')
import Data.Semigroup
import Data.Function (on)
import Data.Vector (Vector)
import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as VU
import Linear
import Quern.Render.StaticScene -- static instance
import Quern.Util (smoothstep, saturate)

-- | @Curve@ accepts a parameter [0..1] and produces a value that varies
-- over this parameter by 'tweaking' a base value.
--
-- TODO: This looks a bit like Endo with a thingy dangling on the front.
newtype Curve a = Curve { runCurve :: Float -> a -> a }

-- | Ignores parameter and input.
--
-- TODO: this violates some assumptions about
-- 'pipelined' curves. Decide if this is OK.
constCurve :: a -> Curve a
constCurve a = Curve (\_ _ -> a)

-- | Identity curve, returns input unchanged
idCurve :: Curve a
idCurve = Curve $ const id

-- | Curve semigroup is just Endo.
instance Semigroup (Curve a) where
  (Curve f) <> (Curve g) = Curve $ \t -> f t . g t

instance Monoid (Curve a) where
  mempty = idCurve

-- | Dubious, breaks associativity.
instance Num a => Num (Curve a) where
  (Curve f) + (Curve g) = Curve $ \t x -> f t x + g t x
  (Curve f) - (Curve g) = Curve $ \t x -> f t x + g t x
  (Curve f) * (Curve g) = Curve $ \t x -> f t x * g t x
  abs (Curve f) = Curve $ \t x -> abs (f t x)
  signum (Curve f) = Curve $ \t x -> signum (f t x)
  negate (Curve f) = Curve $ \t x -> negate (f t x)
  fromInteger = constCurve . fromInteger

-- | Apply a smoothstep to a curve.
smooth :: Curve a -> Curve a
smooth (Curve f) = Curve $ \t -> f (smoothstep t)

-- | Ease in with a power curve
easeIn :: Int -> Curve a -> Curve a
easeIn p (Curve f) = Curve $ \t -> f (t^p)

-- | Ease out with a power curve
easeOut :: Int -> Curve a -> Curve a
easeOut p (Curve f) = Curve $ \t -> let t' = 1 - t in f (1 - t'^p)

-- | A Curve that plays backwards:
-- at parameter = 0 it produces the 'final' value
-- at parameter = 1 it produces the 'initial' value
backwards :: Curve a -> Curve a
backwards (Curve f) = Curve $ \t -> f (1 - t)

-- | Produces a Curve that runs forward to the midpoint,
-- then backwards.
bounce :: Float -> Curve a -> Curve a
bounce mid a@(Curve f)
  | mid <= 0 = backwards a
  | mid >= 1 = a
  | otherwise = Curve $ \t -> f (bnc t)
  where
    bnc x
      | x < mid = x / mid
      | x > mid = 1 - (x-mid)/(1-mid)
      | otherwise = 1

-- | Takes a section of a curve and scales the parameter to fit the [0..1] range.
--
-- Both start and stop points are saturated to [0..1], but
-- the stop point may be less than the start point to reverse
-- the cropped section.
crop :: Float -> Float -> Curve a -> Curve a
crop start stop c@(Curve f)
  | start <= 0 && stop >= 1 = c
  | start >= 1 && stop <= 0 = backwards c
  | abs dur < 1e-5 = Curve $ const atBegin
  | otherwise = Curve $ \t -> f (start' + saturate t * dur)
  where
    atBegin = f start'
    start' = saturate start
    stop' = saturate stop
    dur = stop' - start'

-- | If we can Curve sub-part(s) of something, we can Curve the entire thing
curveIn :: Traversal' s a -> Curve a -> Curve s
curveIn l (Curve f) = Curve $ \t s -> s & l %~ f t

-- | Linear interpolation between two values
--
-- >>> runCurve (tween (V3 0 0 0) (V3 2 0 4)) 0.5 (V3 0 1 0)
--
-- >>> V3 1.0 1.0 2.0
tween :: (Additive f) => f Float -> f Float -> Curve (f Float)
tween a b = Curve $ \t c -> lerp t (c ^+^ ba) c
  where ba = b ^-^ a

-- | Interpolates between two factors that are then applied
-- multiplicatively to the curve value.
--
-- TODO: Breaks associativity
tweenMultiplier :: (Num (f Float), Additive f) => f Float -> f Float -> Curve (f Float)
tweenMultiplier a b = Curve $ \t c -> c * lerp t b a

-- | Slerp between two rotations
tweenRotation :: Quaternion Float -> Quaternion Float -> Curve (Quaternion Float)
tweenRotation a b = Curve $ \x c -> slerp' c (c * ab) x
  where
    ab = b / a

slerp' :: RealFloat a => Quaternion a -> Quaternion a -> a -> Quaternion a
slerp' a b
  | a `dot` b < 0 = slerp (negate a) b
  | otherwise = slerp a b

-- | Produces a Curve that takes the transform of one instance to another
tweenInstance :: Instance -> Instance -> Curve Instance
tweenInstance a b = Curve $ \t i -> let q = i ^. instanceRotation in i
    & instancePosition +~ t *^ dpos
    & instanceScale +~ t *^ dscale
    & instanceRotation .~ (slerp q (q*drot) t)
  where
    dpos = b^.instancePosition - a^.instancePosition
    dscale = b^.instanceScale - a^.instanceScale
    aq = a^.instanceRotation
    bq = b^.instanceRotation
    drot = bq/aq --if aq `dot` bq < 0 then negate bq / aq else bq / aq

-- | `parabola h` describes a curve that achieves a height of `h` at the midpoint
parabola :: Float  -> Curve Float
parabola height = Curve $ \t y -> y + height * (1 - (2 * (t-0.5))^(2::Int))

-- | `squish` is a unit-volume-preserving scale in Z
-- maximum squish is achieved at the end of the animation
squish :: Float -> Curve (V3 Float)
squish z
  | z <= 0 = constCurve 1
  | otherwise = Curve $ \t v -> v & _z +~ dz * t & _xy +~ pure (dxy * t)
  where
    dz = z - 1
    xy = sqrt (recip z)
    dxy = xy - 1

-- | A single full rotation about an axis
roll :: V3 Float -> Curve (Quaternion Float)
roll ax = Curve $ \t q -> q * axisAngle ax (pi*t*2)

class HasBeginTime a where
  beginTime :: Lens' a Double

class HasEndTime a where
  endTime :: Lens' a Double

class HasDuration a where
  duration :: Lens' a Double

newtype Duration = Duration Double deriving (Eq, Ord, Show, Read)
newtype Begin = Begin Double deriving (Eq, Ord, Show, Read)

instance HasBeginTime Begin where
  beginTime = lens (\(Begin t) -> t) (\_ t -> Begin t)

instance HasDuration Duration where
  duration = lens (\(Duration t) -> t) (\_ t -> Duration t)

-- making these begin/end would permit a nicer interval formulation
data Clip a = Clip
  { _clipBegin :: !Begin
  , _clipDuration :: !Duration
  , _clipCurve :: Curve a
  }

clipDuration :: Lens' (Clip a) Double
clipDuration = lens
  (\(Clip _ (Duration d) _) -> d)
  (\c d -> c{_clipDuration = Duration d})

clipBegin :: Lens' (Clip a) Double
clipBegin = lens (\(Clip (Begin b) _ _) -> b) (\c b -> c{ _clipBegin = Begin b })

-- should this scale or shift?
clipEnd :: Lens' (Clip a) Double
clipEnd = lens
  (\(Clip (Begin b) (Duration d) _) -> b + d)
  (\c e -> let e' = max (c^.clipBegin) e in c{ _clipDuration = Duration (e' - c^.clipBegin) })

clipEval :: Double -> Clip a -> a -> a
clipEval t (Clip (Begin t0) (Duration tl) (Curve adj)) a = adj t' a
  where
    t' = saturate $ realToFrac ((t - t0) * rcpLen)
    rcpLen = if tl <= 0 then 0 else recip tl

instance HasDuration (Clip a) where duration = clipDuration
instance HasBeginTime (Clip a) where beginTime = clipBegin
instance HasEndTime (Clip a) where endTime = clipEnd

mkClip :: Begin -> Duration -> Curve a -> Clip a
mkClip b d adj = Clip
  { _clipBegin = b
  , _clipDuration = d
  , _clipCurve = adj
  }

mkClip01 :: Curve a -> Clip a
mkClip01 = mkClip (Begin 0) (Duration 1)

clipIn :: Traversal' s a -> Clip a -> Clip s
clipIn t (Clip b d f) = Clip b d (curveIn t f)

shiftBeginning :: Double -> Clip a -> Clip a
shiftBeginning delta c = c & clipBegin +~ delta

--
-- does storing the begin/duration make any sense here?
newtype Animation a = Animation
  { _animationClips :: (Vector (Clip a))
  }

animBegin :: Animation a -> Double
animBegin (Animation cs)
  | null cs = 0
  | otherwise =  (^.clipBegin) $ V.minimumBy (compare `on` (^. clipBegin)) cs

animEnd :: Animation a -> Double
animEnd (Animation cs)
  | null cs = 0
  | otherwise =  (^.clipEnd) $ V.maximumBy (compare `on` (^. clipEnd)) cs

animationIn :: Traversal' s a -> Animation a -> Animation s
animationIn t (Animation cs) = Animation (clipIn t <$> cs)

-- | Access the start time of the animation
animationBegin :: Lens' (Animation a) Double
animationBegin = lens animBegin $ \a@(Animation cs) b ->
    let b0 = animBegin a
    in if null cs then a else Animation (shiftBeginning (b - b0) <$> cs) -- (shiftEvents (b-b0) es)

-- | Access the end time of the animation, rescaling all clips
-- to fit
animationEnd :: Lens' (Animation a) Double
animationEnd = lens animEnd $ \a@(Animation cs) e ->
  if null cs
      then a
      else
        let b = animBegin a
            d = animDur a
            r = if d <= 0 then 0 else (e-b) / d
        in  Animation (fmap (\c -> c & clipBegin *~ r & clipDuration *~ r) cs)

animDur :: Animation a -> Double
animDur (Animation cs) = case beginEnd of
    Just (Min b, Max e) -> e - b
    Nothing -> 0
  where
    beginEnd = foldMap (\c -> Just (Min (c^.clipBegin), Max (c^.clipEnd))) cs

animationDuration :: Lens' (Animation a) Double
animationDuration = lens animDur $ \a@(Animation cs) d1 ->
    let d0 = animDur a
        ratio = if d0 <= 0 then 0 else d1 / d0
    in Animation ((clipDuration *~ ratio) <$> cs)


instance HasDuration (Animation a) where duration = animationDuration
instance HasBeginTime (Animation a) where beginTime = animationBegin
instance HasEndTime (Animation a) where endTime = animationEnd

mkAnimation :: Clip a -> Animation a
mkAnimation c = Animation (V.singleton c)

mkAnimation01 :: Curve a -> Animation a
mkAnimation01 = mkAnimation . mkClip01

animationEval :: Double -> Animation a -> a -> a
animationEval t (Animation cs) x = foldl' (\y c -> clipEval t c y) x cs

animationEmpty :: Animation a -> Bool
animationEmpty = null . _animationClips

idAnimation :: Double -> Animation a
idAnimation d = mkAnimation $ mkClip (Begin 0) (Duration d) idCurve

maybeAnimation :: Animation a -> Maybe (Animation a)
maybeAnimation a@(Animation cs)
  | null cs = Nothing
  | otherwise = Just a

-- | Given a time value, produces an @Animation@ that only has running or
-- yet-to-be-started clips, and a vector of clips that have finished running
pruneAnimation :: Double -> Animation a -> (Animation a, Vector (Clip a))
pruneAnimation t (Animation cs) = (Animation extant, finished)
  where
    (extant, finished) = V.partition good cs
    good c = c^.clipEnd >= t

-- | @Animation@ semigroup action composes contained clips in parallel
instance Semigroup (Animation a) where
  (Animation c0) <> (Animation c1) = Animation (c0 <> c1)

instance Monoid (Animation a) where
  mempty = Animation mempty

-- | @a `after` b@ plays all the clips in animation @b@, then all the clips in @a@
after :: Animation a -> Animation a -> Animation a
after a b = b <> (a & animationBegin .~ animEnd b)

infixr 1 `after`

-- | @sequentially a b@ plays all the clips in animation @a@, then all those in @b@
sequentially :: Animation a -> Animation a -> Animation a
sequentially = flip after
