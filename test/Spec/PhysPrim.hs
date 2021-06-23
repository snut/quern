module Spec.PhysPrim where


import Linear
import Hedgehog
import Hedgehog.Gen as Gen hiding (scale)
import Hedgehog.Range as Range hiding (origin)

import Quern.Physics.Primitive

import Spec.Util
import Data.Maybe (isJust, isNothing)


-- generate a shape randomly positioned in a volume
-- with random rotation and scale between (0.125 .. 2)
genShape :: MonadGen m => V3 Float -> V3 Float -> m Shape
genShape origin extent = do
  let f = float (linearFrac (-1) 1)
      s = float (linearFrac 0.125 2.0)
  upos <- V3 <$> f <*> f <*> f
  let pos = upos * extent + origin
  rot <- genQuat
  choice
    [ Sphere pos <$> s
    , Box pos rot <$> (V3 <$> s <*> s <*> s)
    , Cylinder pos rot <$> (V2 <$> s <*> s)
    ]

-- put shapes at origin, ensure they are always hit
prop_alwaysHitOrigin :: Property
prop_alwaysHitOrigin = property $ do
  s <- forAll $ genShape 0 0
  v <- forAll genUnitV3
  let w = PhysicsWorld [TaggedShape () s]
      h = rayTest w $ rayBetween (v ^* 10) 0
  assert $ isJust h

-- put a shape at a distant point
prop_alwaysMissDistant :: Property
prop_alwaysMissDistant = property $ do
  s <- forAll $ genShape 20 0
  v <- forAll genUnitV3
  let w = PhysicsWorld [TaggedShape () s]
      h = rayTest w $ rayBetween (v ^* 10) 0
  assert $ isNothing h

-- check property
-- sample generator
