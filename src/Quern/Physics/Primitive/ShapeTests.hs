{-| The module is unfinished. GJK probably makes a lot more sense than this mess.
-}

module Quern.Physics.Primitive.ShapeTests where
-- ()

import Linear
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Quern.Physics.AxisAlignedBox
import Quern.Physics.Primitive.Types
import Quern.Physics.Primitive.RayTests


shapeTest :: PhysicsWorld a -> TaggedShape a -> V.Vector (CollisionResult a)
shapeTest (PhysicsWorld shapes) s@(TaggedShape ta sa boxa) = foldMap go shapes
  where
    go (TaggedShape tb sb boxb)
      | not (boxa `overlaps` boxb) = mempty
      | otherwise = case shapeShapeTest sa sb of
        Just h -> V.singleton (CollisionResult ta tb h)
        _ -> mempty

shapeShapeTest :: Shape -> Shape -> Maybe Contact
shapeShapeTest (Sphere spos srad _) shape = sphereShape spos srad shape
shapeShapeTest shape (Sphere spos srad _) = flipContact <$> sphereShape spos srad shape
shapeShapeTest (Box bpos brot (S bext)) shape = boxShape bpos brot bext shape
shapeShapeTest shape (Box bpos brot (S bext)) = flipContact <$> boxShape bpos brot bext shape
shapeShapeTest (Capsule cpos crot crad _) shape = capsuleShape cpos crot crad shape
shapeShapeTest shape (Capsule cpos crot crad _) = flipContact <$> capsuleShape cpos crot crad shape
shapeShapeTest (Cylinder cpos crot crad _) shape = cylinderShape cpos crot crad shape
shapeShapeTest shape (Cylinder cpos crot crad _) = flipContact <$> cylinderShape cpos crot crad shape
shapeShapeTest _ _ = Nothing

-- shape/shape
sphereShape :: V3 Float -> Float -> Shape -> Maybe Contact
sphereShape spos srad shp = case shp of
  Sphere spos' srad' _ -> sphereSphere spos srad spos' srad'
  Box bpos brot (S bext) -> sphereBox spos srad bpos brot bext
  Capsule cpos crot crad _ -> sphereCapsule spos srad cpos crot crad
  Cylinder cpos crot crad _ -> sphereCylinder spos srad cpos crot crad
  Hull hverts hplanes -> sphereHull spos srad hverts hplanes
  _ -> Nothing

sphereSphere :: V3 Float -> Float -> V3 Float -> Float -> Maybe Contact
sphereSphere a ar b br
  | qd a b <= ((br+ar)*(br+ar)) = Just $ Contact{ _contactPoint = (a+b+n^*(ar-br)) ^* 0.5, _contactNormal = n }
  | otherwise = Nothing
  where
    n = ba ^/ lenBA
    ba = b - a
    lenBA = norm ba

sphereBox :: V3 Float -> Float -> V3 Float -> Quaternion Float -> V3 Float -> Maybe Contact
sphereBox spos sr bpos brot bext
  | maximum delta >= 0 = Nothing
  | dx <= dy && dx <= dz = Just $ Contact { _contactPoint = cpnt, _contactNormal = rotate brot (V3 nx 0 0) }
  | dy <= dz = Just $ Contact { _contactPoint = cpnt, _contactNormal = rotate brot (V3 0 ny 0) }
  | otherwise = Just $ Contact {_contactPoint = cpnt, _contactNormal = rotate brot (V3 0 0 nz) }
  where
    spos' = rotate (conjugate brot) (spos - bpos)
    n@(V3 nx ny nz) = signum <$> spos'
    delta@(V3 dx dy dz) = abs spos' - (bext + pure sr)
    cpnt = bpos + rotate brot (signum spos' * min bext (abs spos'))

sphereCapsule :: V3 Float -> Float -> V3 Float -> Quaternion Float -> V2 Float -> Maybe Contact
sphereCapsule _spos _sr _cpos _crot _crad = Nothing

sphereCylinder :: V3 Float -> Float -> V3 Float -> Quaternion Float -> V2 Float -> Maybe Contact
sphereCylinder _spos _sr _cpos _crot _crad = Nothing

sphereHull :: V3 Float -> Float -> VS.Vector (V3 Float) -> VS.Vector (V4 Float) -> Maybe Contact
sphereHull _spos _sr _hverts _hplanes = Nothing

boxShape :: V3 Float -> Quaternion Float -> V3 Float -> Shape -> Maybe Contact
boxShape bpos brot bext shape = case shape of
  Box bpos' brot' bext' -> Nothing
  Capsule{} -> Nothing
  Cylinder{} -> Nothing
  Hull{} -> Nothing
  _ -> Nothing

{-
boxBox
boxCapsule
boxCylinder
boxHull
-}

capsuleShape :: V3 Float -> Quaternion Float -> V2 Float -> Shape -> Maybe Contact
capsuleShape cpos crot crad shape = case shape of
  Capsule{} -> Nothing
  Cylinder{} -> Nothing
  Hull{} -> Nothing
  _ -> Nothing

{-
capsuleCapsule
capsuleCylinder
capsuleHull
-}

cylinderShape :: V3 Float -> Quaternion Float -> V2 Float -> Shape -> Maybe Contact
cylinderShape cpos crot crad shape = case shape of
  Cylinder{} -> Nothing
  Hull{} -> Nothing
  _ -> Nothing

{-
cylinderCylinder
cylinderHull
-}

hullShape :: V3 Float -> Quaternion Float -> VS.Vector (V3 Float) -> VS.Vector (V4 Float) -> Shape -> Maybe Contact
hullShape _hpos _hrot _hverts _hplanes shape = case shape of
  Hull{} -> Nothing
  _ -> Nothing

{-
hullHull ::
-}
