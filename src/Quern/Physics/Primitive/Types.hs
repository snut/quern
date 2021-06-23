{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-| Mostly for supporting ray tests used for selection etc -}

module Quern.Physics.Primitive.Types where
--()

import Control.Lens
import Linear
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign hiding (rotate)
import Quern.Physics.AxisAlignedBox
import Data.Maybe (fromJust)

-- Might have flipped convention on ray-vs-line here...
-- rays are infinite, defined by a point and a direction
-- the reciprocal direction is stored as an optimisation, please don't do bad things
-- like make it inconsistent
data Ray = Ray
  { _rayPoint :: !(V3 Float)
  , _rayDir :: !(V3 Float)
  , _rayRcpDir :: !(V3 Float)
  } deriving (Eq, Ord, Show, Read)
makeLenses ''Ray

rayEval :: Ray -> Float -> V3 Float
rayEval (Ray o d _) l = o + d ^* l

instance Storable Ray where
  sizeOf _ = sizeOf (undefined :: V3 Float) * 3
  alignment _ = alignment (undefined :: V3 Float)
  peek ptr = do
    let v3 = castPtr ptr
    pos <- peek v3
    dir <- peek (advancePtr v3 1)
    rcp <- peek (advancePtr v3 2)
    pure (Ray pos dir rcp)
  poke ptr (Ray pos dir rcp) = do
    let v3 = castPtr ptr
    poke v3 pos
    poke (advancePtr v3 1) dir
    poke (advancePtr v3 2) rcp


data Line = Line
  { _lineBegin :: !(V3 Float)
  , _lineEnd :: !(V3 Float)
  } deriving (Eq, Ord, Show, Read)
makeLenses ''Line

lineEval :: Line -> Float -> V3 Float
lineEval (Line b e) x
  | x <= 0 = b
  | x >= 1 = e
  | otherwise = lerp x e b

instance Storable Line where
  sizeOf _ = sizeOf (undefined :: V3 Float) * 2
  alignment _ = alignment (undefined :: V3 Float)
  peek ptr = do
    let v3 = castPtr ptr
    a <- peek v3
    b <- peek (advancePtr v3 1)
    pure $ Line a b
  poke ptr (Line a b) = do
    let v3 = castPtr ptr
    poke v3 a
    poke (advancePtr v3 1) b

mkRay :: V3 Float -> V3 Float -> Ray
mkRay pos dir
  | quadrance dir < 1e-5 = Ray pos 0 (1/0)
  | otherwise = let n = normalize dir in Ray pos n (recip n)

-- | `rayBetween start end` makes a ray with origin at `start`, pointing towards `end`
rayBetween :: V3 Float -> V3 Float -> Ray
rayBetween start end = mkRay start (end - start)

mkLine :: V3 Float -> V3 Float -> Line
mkLine begin end = Line begin end

rayToLine :: Ray -> Float -> Line
rayToLine (Ray o d _) len = Line o (o + d ^* len)

--
-- | Data for reporting ray-shape intersection
data Hit = Hit
  { _hitDistance :: !Float
  , _hitPoint :: !(V3 Float)
  , _hitNormal :: !(V3 Float)
  } deriving (Eq, Ord, Show, Read)
makeLenses ''Hit

newtype Far = Far { _unFar :: Hit } deriving (Eq, Show, Read)
instance Ord Far where
  (Far a) < (Far b) = a >= b
  (Far a) <= (Far b) = a > b
  (Far a) > (Far b) = a <= b
  (Far a) >= (Far b) = a < b

-- | Data for reporting shape-shape intersection
data Contact = Contact
  { _contactPoint :: !(V3 Float)
  , _contactNormal :: !(V3 Float)
  } deriving (Eq, Ord, Show, Read)
makeLenses ''Contact

flipContact :: Contact -> Contact
flipContact c = c{ _contactNormal = negate (_contactNormal c) }

instance Semigroup Hit where
  a <> b = if _hitDistance a <= _hitDistance b && _hitDistance a >= 0 then a else b

instance Semigroup Far where
  a@(Far ha) <> b@(Far hb) = if _hitDistance ha > _hitDistance hb then a else b

furthest :: Maybe Hit -> Maybe Hit -> Maybe Hit
furthest a b = _unFar <$> (Far <$> a) <> (Far <$> b)

newtype Scale3 a = S (V3 a)
  deriving (Eq, Ord, Show, Read, Functor, Applicative, Additive, Num, Fractional)

idScale :: Num a => Scale3 a
idScale = pure 1

-- | Simple shapes to perform tests against
data Shape
  = Sphere !(V3 Float) !Float (Scale3 Float)
  | Box !(V3 Float) !(Quaternion Float) !(Scale3 Float)
  | Capsule !(V3 Float) !(Quaternion Float) !(V2 Float) !(Scale3 Float)
  | Cylinder !(V3 Float) !(Quaternion Float) !(V2 Float) !(Scale3 Float)
  | Hull !(VS.Vector (V3 Float)) !(VS.Vector (V4 Float))
  deriving (Eq, Ord, Show, Read)

mkAABB :: Shape -> AABox V3 Float
mkAABB (Sphere p r (S s)) = scaleBox s . grow r $ degenerate p
mkAABB (Box p r (S s)) = fromJust $ foldAABox (\v -> p + rotate r (s*v)) [V3 x y z | x <- [-1,1], y <- [-1,1], z <- [-1,1]]
mkAABB (Capsule p q (V2 r h) (S s)) = let o = rotate q (V3 0 0 1) ^* h in scaleBox s $ grow r (mkAABox (p+o) (p-o))
mkAABB (Cylinder p q (V2 r h) (S s)) = let o = rotate q (V3 0 0 1) ^* h in scaleBox s $ grow r (mkAABox (p+o) (p-o))
mkAABB (Hull verts _planes) = VS.foldl1 (<>) (VS.map degenerate verts)

data TaggedShape a = TaggedShape
  { _taggedTag :: !a
  , _taggedShape :: !Shape
  , _taggedAABB :: !(AABox V3 Float)
  } deriving (Eq, Ord, Show, Read)
makeLenses ''TaggedShape

tagShape :: a -> Shape -> TaggedShape a
tagShape tag shape = TaggedShape tag shape (mkAABB shape)

instance Functor TaggedShape where
  fmap f (TaggedShape t shape aabb) = TaggedShape (f t) shape aabb


-- TODO: Replace with BVH or similar for more complex worlds
newtype PhysicsWorld a = PhysicsWorld
  { _physicsShapes :: V.Vector (TaggedShape a)
  } deriving (Eq, Ord, Show, Read)

instance Functor PhysicsWorld where
  fmap f (PhysicsWorld ss) = PhysicsWorld (fmap f <$> ss)

instance Semigroup (PhysicsWorld a) where
  (PhysicsWorld a) <> (PhysicsWorld b) = PhysicsWorld (a <> b)

instance Monoid (PhysicsWorld a) where
  mempty = PhysicsWorld mempty

data RayResult a = RayResult
  { _resultTag :: !a
  , _resultHit :: !Hit
  } deriving (Eq, Ord, Show, Read)
makeLenses ''RayResult

data CollisionResult a = CollisionResult
  { _collisionTagA :: !a
  , _collisionTagB :: !a
  , _collisionContact :: !Contact
  } deriving (Eq, Ord, Show, Read)
makeLenses ''CollisionResult

-- | `RayResult` has a semigroup that yields the 'nearest' of two
-- results. The assumption is that if the same ray produces multiple hits, we return the
-- one with the smallest parameter value.
--
-- This is not a reasonable assumption for two general results
instance Semigroup (RayResult a) where
  a <> b = if aP <= bP && aP >= 0 then a else b
    where
      aP = _hitDistance (_resultHit a)
      bP = _hitDistance (_resultHit b)

-- | During physics it can be interesting to report when broadphase collision triggers
-- even if the narrow phase fails.
--
-- `Nope` indicates a complete miss, two primitives whose bounds are not close
-- `Nearly` indicates that e.g. two shapes' bounding boxes overlap, but the shapes do not
-- `Actually` indicates a resolved collision
--
-- This is isomorphic to `Maybe (Maybe a)` but with friendly names
data Nearly a
  = Nope
  | Nearly
  | Actually a
  deriving (Eq, Ord, Show, Read)

nearlyToMaybe :: Nearly a -> Maybe a
nearlyToMaybe (Actually a) = Just a
nearlyToMaybe _ = Nothing

instance Semigroup a => Semigroup (Nearly a) where
  Nope <> o = o
  Actually a <> Actually b = Actually (a <> b)
  a@Actually{} <> _ = a
  _ <> b@Actually{} = b
  Nearly <> _ = Nearly

instance Semigroup a => Monoid (Nearly a) where
  mempty = Nope

instance Functor Nearly where
  fmap f (Actually a) = Actually (f a)
  fmap _ Nearly = Nearly
  fmap _ Nope = Nope

instance Applicative Nearly where
  (Actually f) <*> (Actually a) = Actually (f a)
  Actually{} <*> Nearly = Nearly
  Nearly <*> Actually{} = Nearly
  Nearly <*> Nearly = Nearly
  _ <*> _ = Nope
  pure = Actually

-- Can't think of a sane use for this, but just in case...
instance Monad Nearly where
  (Actually a) >>= k = k a
  Nearly >>= _ = Nearly
  _ >>= _ = Nope
