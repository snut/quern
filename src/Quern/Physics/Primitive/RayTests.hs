module Quern.Physics.Primitive.RayTests where
-- ()

import Linear
import Control.Lens ((^.))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Quern.Physics.AxisAlignedBox
import Quern.Physics.Primitive.Types

rayTest :: PhysicsWorld a -> Ray -> Nearly (RayResult a)
rayTest (PhysicsWorld shapes) r = foldMap go shapes
  where
    go (TaggedShape t x aabb)
      | not (aabbRayCollide r aabb) = Nope
      | otherwise = case rayShapeTest r x of
                      Just h -> Actually (RayResult t h)
                      Nothing -> Nearly
--


rayShapeTest :: Ray -> Shape -> Maybe Hit
rayShapeTest ray shape = case shape of
  Sphere pos radius (S scl) -> sphereRayTest ray pos radius
  Box pos rot (S extent) -> boxRayTest ray pos rot extent
  Capsule pos rot radiusLen (S scl) -> capsuleRayTest ray pos rot radiusLen
  Cylinder pos rot radiusLen (S scl) -> cylinderRayTest ray pos rot radiusLen
  Hull _ planes -> hullRayTest ray planes

--

-- shape/ray
sphereRayTest :: Ray -> V3 Float -> Float -> Maybe Hit
sphereRayTest (Ray o d _) pos radius
  | distToRaySq > radSq = Nothing
  | param >= 0 = Just h
  | otherwise = Nothing
  where
    radSq = radius * radius
    l = pos - o
    ld = l `dot` d
    onRay = d ^* ld + o
    distToRaySq = qd onRay pos
    delta = sqrt (radSq - distToRaySq)
    i0 = onRay - d ^* delta
    i1 = onRay + d ^* delta
    -- test if ray starts inside sphere
    i = if qd o pos <= radSq then i1 else i0
    param = (i - o) `dot` d
    h = Hit { _hitPoint = i
            , _hitNormal = normalize (i - pos)
            , _hitDistance = param }

-- aabb at origin
acceptBox :: V3 Float -> V3 Float -> Maybe Hit -> Maybe Hit
acceptBox _ _ Nothing = Nothing
acceptBox bmin bmax h@(Just hit)
  | and ((>=) <$> p <*> bmin) && and ((<=) <$> p <*> bmax) = h
  | otherwise = Nothing
  where
    p = _hitPoint hit


aabbRayTest :: Ray -> V3 Float -> V3 Float -> Maybe Hit
aabbRayTest r bmin@(V3 mnx mny mnz) bmax@(V3 mxx mxy mxz) = hitx <> hity <> hitz
  where
    unitAxes@(V3 x1 y1 z1) = identity
    V3 xe ye ze = unitAxes ^* 1e-5
    w (V3 x y z) = V4 x y z
    hitx = acceptBox (bmin - xe) (bmax + xe) $ planeTest r (w x1 mxx) <> planeTest r (negate $ w x1 mnx)--(V4 (-1) 0 0 (-mnx))
    hity = acceptBox (bmin - ye) (bmax + ye) $ planeTest r (w y1 mxy) <> planeTest r (negate $ w y1 mny)--(V4 0 (-1) 0 (-mny))
    hitz = acceptBox (bmin - ze) (bmax + ze) $ planeTest r (w z1 mxz) <> planeTest r (negate $ w z1 mnz)--(V4 0 0 (-1) (-mnz))

boxRayTest :: Ray -> V3 Float -> Quaternion Float -> V3 Float -> Maybe Hit
boxRayTest ray pos rot ext = aabbRayTest (Ray o d d') (negate ext) ext
  where
    r = conjugate rot
    o = rotate r (_rayPoint ray - pos)
    d = rotate r (_rayDir ray)
    d' = recip d

planeTest :: Ray -> V4 Float -> Maybe Hit
planeTest (Ray o d _) pln
  | d_n >= 0 || nearZero d_n = Nothing
  | otherwise = Just h
  where
    d_n = d `dot` n
    n = pln ^. _xyz
    o_n = o `dot` n - pln^._w
    l = negate (o_n / d_n)
    h = Hit { _hitPoint = o + d ^* l
            , _hitNormal = n
            , _hitDistance = l }

acceptRadius :: V3 Float -> Float -> Maybe Hit -> Maybe Hit
acceptRadius p r m@(Just h)
  | qd (_hitPoint h) p <= r*r  = m
  | otherwise = Nothing
acceptRadius _ _ m = m

appV :: V3 a -> a -> V4 a
appV (V3 x y z) w = V4 x y z w


circleRayTest :: V2 Float -> V2 Float -> Float -> Maybe (Float, Float)
circleRayTest (V2 ox oy) (V2 dx dy) r
  | rdcl < 0 || abs qa < 1e-6 = Nothing
  | otherwise = Just (t0, t1)
  where
    -- solve (t*dx+ox, t*dy+oy)^2 - r^2 = 0
    -- (t*dx+ox)*(t*dx+ox)+(t*dy+oy)*(t*dy+oy)
    -- (t*dx)^2+2(t*dx*ox)+ox^2 + ...
    -- t^2*(dx^2 + dy^2) + 2t*(dx*ox+dy*oy) + ox^2+oy^2 = 0
    qa = dx*dx + dy*dy
    qb = 2*(dx*ox + dy*oy)
    qc = ox*ox + oy*oy - r*r
    rdcl = qb*qb - 4*qa*qc
    srdcl = sqrt rdcl
    t0 = (negate qb + srdcl) / (2*qa)
    t1 = (negate qb - srdcl) / (2*qa)

cylinderWallTest :: Ray -> V3 Float -> Quaternion Float -> V2 Float -> V3 Float -> Maybe Hit
cylinderWallTest ray@(Ray o d _) pos rot (V2 r h) n = sides
    where
    -- put the plane into a space where the cylinder is at the origin & aligned with Z
      crot = conjugate rot
      V3 ox oy oz = rotate crot (o - pos)
      V3 dx dy dz = rotate crot d

      sides = case circleRayTest (V2 ox oy) (V2 dx dy) r of
        Nothing -> Nothing
        Just (t0, t1) -> root t0 <> root t1

      root t
        | abs (oz + dz*t) > h = Nothing
        | otherwise = Just $ Hit t point nrm
        where
          point = o + d ^* t
          delta = point - pos
          nrm = normalize (delta - (n ^* (n `dot` delta)))

cylinderRayTest :: Ray -> V3 Float -> Quaternion Float -> V2 Float -> Maybe Hit
cylinderRayTest ray pos rot sz@(V2 r h) = top <> btm <> sides
  where
    n = rotate rot (V3 0 0 1)
    v = negate n
    prj = n `dot` pos
    top = acceptRadius (pos + n^*h) r $ planeTest ray (appV n (h + prj))
    btm = acceptRadius (pos - n^*h) r $ planeTest ray (appV v (h - prj))
    sides = cylinderWallTest ray pos rot sz n



capsuleRayTest :: Ray -> V3 Float -> Quaternion Float -> V2 Float -> Maybe Hit
capsuleRayTest ray pos rot sz@(V2 r h) = top <> btm <> sides
  where
    n = rotate rot (V3 0 0 1)
    top = sphereRayTest ray (pos + n^*h) r
    btm = sphereRayTest ray (pos - n^*h) r
    sides = cylinderWallTest ray pos rot sz n

hullRayTest :: Ray -> VS.Vector (V4 Float) -> Maybe Hit
hullRayTest ray@(Ray o d rc) planes = case (near, far) of
    (Just hn, Just hf) | negate (_hitDistance hf) >= _hitDistance hn -> Just hn
    _ -> Nothing
  where
    near = VS.foldl' (\a -> furthest a . planeTest ray) Nothing planes
    far  = VS.foldl' (\a -> furthest a . planeTest ray') Nothing planes
    ray' = Ray o (negate d) (negate rc)

-- acceleration structures
aabbRayCollide :: Ray -> AABox V3 Float -> Bool
aabbRayCollide (Ray pnt dir rcp) box = mx >= mn
  where
    t0 = (aaBoxMin box - pnt) * rcp
    t1 = (aaBoxMax box - pnt) * rcp
    mn = maximum $ min <$> t0 <*> t1
    mx = minimum $ max <$> t0 <*> t1
