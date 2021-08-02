{--}

module Quern.Render.Instance where

import Control.Lens hiding (ix, re)
import Foreign hiding (rotate)
import Linear

class HasTransform a where
  rotation :: Lens' a (Quaternion Float)
  position :: Lens' a (V3 Float)
  scale :: Lens' a (V3 Float)

-- | I don't much like this, a pos+rot+scale+inverse-scale representation would
-- be nicer in many ways.
data Instance = Instance
  { _instanceTransform :: !(V3 (V4 Float))
  , _instanceTint :: !(V4 Float) -- this is just padding, really
  } deriving (Eq, Ord, Show, Read)

transformPos :: Instance -> V3 Float -> V3 Float
transformPos (Instance (V3 tx ty tz) _) (V3 px py pz) = p'^._xyz
  where
    p' = m !* (V4 px py pz 1)
    m = V4 tx ty tz (V4 0 0 0 1)

instanceTransform :: Lens' Instance (V3 (V4 Float))
instanceTransform = lens prj inj
  where
    prj (Instance t _) = t
    inj i t = i{ _instanceTransform = t }

instanceTint :: Lens' Instance (V4 Float)
instanceTint = lens prj inj
  where
    prj (Instance _ c) = c
    inj i c = i{ _instanceTint = c}


mkInstance :: Quaternion Float -> V3 Float -> V3 Float -> Instance
mkInstance rot pos scl = Instance (V3 x y z) 1
  where
    m = mkTransformation (normalize rot) pos
    s = m33_to_m44 (scaled scl)
    V4 x y z _ = m !*! s

instancePosition :: Lens' Instance (V3 Float)
instancePosition = lens prj inj
  where
    prj (Instance t _) = V3 (t ^. _x._w) (t ^._y._w) (t ^._z._w)
    inj st (V3 x y z) = st & instanceTransform._x._w .~ x
                           & instanceTransform._y._w .~ y
                           & instanceTransform._z._w .~ z

instanceScale :: Lens' Instance (V3 Float)
instanceScale = lens prj inj
  where
    prj (Instance t _) = norm <$> t' --norm . (^. _xyz) <$> t
      where t' = transpose $ (^._xyz) <$> t
    inj i s = i & instanceTransform._x._xyz .~ tx
                & instanceTransform._y._xyz .~ ty
                & instanceTransform._z._xyz .~ tz
      where
        t = transpose $ (^._xyz) <$> _instanceTransform i
        (V3 tx ty tz) = transpose (rescale <$> s <*> t)
    rescale s v  = s *^ normalize v

setSignFrom :: (Num a, Ord a) => a -> a -> a
setSignFrom a b
  | b < 0 = negate (abs a)
  | otherwise = abs a


instanceRotation :: Lens' Instance (Quaternion Float)
instanceRotation = lens prj inj
  where
    inj st q = i & instanceTint .~ (st^.instanceTint)
      where i = mkInstance q (st^.instancePosition) (st^.instanceScale)
    prj i@(Instance (V3 x' y' z') _) = normalize $ Quaternion re im
      where
        rscl = recip (i^.instanceScale)
        V3 m00 m01 m02 = x'^._xyz * rscl
        V3 m10 m11 m12 = y'^._xyz * rscl
        V3 m20 m21 m22 = z'^._xyz * rscl
        re = 0.5 * sqrt (max 0 (1 + m00 + m11 + m22))
        ix = 0.5 * sqrt (max 0 (1 + m00 - m11 - m22))
        iy = 0.5 * sqrt (max 0 (1 - m00 + m11 - m22))
        iz = 0.5 * sqrt (max 0 (1 - m00 - m11 + m22))
        im = setSignFrom <$> V3 ix iy iz <*> V3 (m21-m12) (m02-m20) (m10-m01)

instance HasTransform Instance where
  rotation = instanceRotation
  position = instancePosition
  scale = instanceScale

instance Storable Instance where
  sizeOf _ = sizeOf (undefined :: V4 Float) * 4
  alignment _ = 1
  peek ptr = do
    t <- peek (castPtr ptr)
    c <- peek (ptr `plusPtr` (sizeOf (undefined :: V4 Float) * 3))
    pure $ Instance { _instanceTransform = t, _instanceTint = c }
  poke ptr (Instance t c) = do
    poke (castPtr ptr) t
    poke (ptr `plusPtr` (sizeOf (undefined :: V4 Float) * 3)) c
