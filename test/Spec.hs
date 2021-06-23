{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}
{-# Language FlexibleInstances #-}

import Control.Lens
import Linear
import Hedgehog
import Hedgehog.Gen as Gen hiding (scale)
import Hedgehog.Range as Range
import System.Exit (exitFailure, exitSuccess)
import System.IO as IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import Quern.Util
import Quern.Render.Instance

import Spec.Util
import Spec.PhysPrim


genPos, genScale :: MonadGen m => m (V3 Float)
genPos = let p = float (linearFrac (-10) 10) in V3 <$> p <*> p <*> p
genScale = V3 <$> s <*> s <*> s
  where
    s = float (linearFrac 0.125 16)
    -- NB: we do not support negative scaling, as it causes some degree of pain
    --s = bool >>= \n -> if n then negate <$> sa else sa

genInstance :: MonadGen m => m Instance
genInstance = mkInstance <$> genQuat <*> genPos <*> genScale

eqish :: Metric f => f Float -> f Float -> Bool
eqish u v = qd u v < 1e-5

quat_eqish :: Quaternion Float -> Quaternion Float -> Bool
quat_eqish a b = min (qd a b) (qd a (negate b)) < 1e-5

prop_quatBetween :: Property
prop_quatBetween = property $ do
  u <- forAll genUnitV3
  v <- forAll genUnitV3
  let q = quatBetween u v
  assert $ eqish v (rotate q u)

prop_instanceGetters :: Property
prop_instanceGetters = property $ do
  q <- forAll genQuat
  p <- forAll genPos
  s <- forAll genScale
  let i = mkInstance q p s
  assert $
    quat_eqish (i^.rotation) q &&
    eqish (i^.position) p &&
    eqish (i^.scale) s

l_setting :: (Show s, Show a, EqIsh a) => Lens' s a -> Gen s -> Gen a -> Property
l_setting l gs ga = property $ do
  s <- forAll gs
  a <- forAll ga
  let s' = s & l .~ a
  assert $ s'^.l ~= a

prop_instanceRotSet :: Property
prop_instanceRotSet = l_setting rotation genInstance genQuat

prop_instancePosSet :: Property
prop_instancePosSet = l_setting position genInstance genPos

prop_instanceSclSet :: Property
prop_instanceSclSet = l_setting scale genInstance genScale

tests :: IO Bool
tests = checkParallel $$(discover)

overriding :: FilePath -> Handle -> IO a -> IO a
overriding pth h body = do
  h' <- hDuplicate h
  openFile pth WriteMode >>= \f -> hDuplicateTo f h
  result <- body
  hDuplicateTo h' h
  pure result

wrapTests :: IO Bool
wrapTests =
  overriding ".stdout.txt" stdout (overriding ".stderr.txt" stderr tests)

main :: IO ()
main = do
  passed <- wrapTests
  if passed
    then exitSuccess
    else exitFailure
