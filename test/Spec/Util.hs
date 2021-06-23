
{-# Language FlexibleInstances #-}

module Spec.Util where

import Linear

import Hedgehog
import Hedgehog.Gen as Gen hiding (scale)
import Hedgehog.Range as Range

import Quern.Util

class EqIsh a where
  (~=) :: a -> a -> Bool
infix 4 ~=

instance EqIsh (V3 Float) where
  a ~= b = qd a b < 1e-5
instance EqIsh (Quaternion Float) where
  a ~= b = min (qd a b) (qd a (negate b)) < 1e-5


genUnitV3 :: MonadGen m => m (V3 Float)
genUnitV3 = do
  let f = float (linearFrac 0 1)
  canonicalToUnit <$> f <*> f

genQuat :: MonadGen m => m (Quaternion Float)
genQuat = do
  th <- float (linearFrac (-pi) pi)
  v <- genUnitV3
  pure $ axisAngle v th
