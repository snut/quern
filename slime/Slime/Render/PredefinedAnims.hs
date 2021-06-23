module Slime.Render.PredefinedAnims
  where

import Control.Lens hiding (backwards)
import Linear
import System.Random (getStdRandom, random)


import Quern.Types
import Quern.Animation
import Quern.Render.StaticScene
import Quern.Util

import Tiles.Hex


spawnAnim :: Animation Instance
spawnAnim = bouncing `after` dropDown `after` popIn
  where
    bouncePos = mkAnimation01 (curveIn (instancePosition._z) (parabola 0.25))
    bouncing = squidge <> bouncePos & duration .~ 0.25
    dropDown1 = mkAnimation01 (crop 0.5 1 (curveIn (instancePosition._z) (parabola 2)))
    dropDown = dropDown1 & duration .~ 0.5
    popIn1 = mkAnimation01 $ curveIn instanceScale (backwards (smooth (tween 1 0.01)))
    popIn = popIn1 & duration .~ 0.2

delaySpawn :: Double -> Double -> Quern e (Animation Instance)
delaySpawn start rnd = do
  r <- liftIO (getStdRandom random)
  let dly = mkAnimation01 idCurve & duration .~ (start + r * rnd)
  pure (spawnAnim `after` dly)

idleAnim :: Double -> Animation Instance
idleAnim t = animationIn instanceScale breath & beginTime .~ t
  where
    rest = mkAnimation01 idCurve & duration .~ 0.5
    inhale = mkAnimation01 . bounce 0.66 $ squish 0.95
    exhale = mkAnimation01 . bounce 0.33 $ squish 1.05
    breath = ((exhale `after` inhale) `after` rest) & duration .~ 1.93

squidge :: Animation Instance
squidge = o `after` i
  where
    i = mkAnimation (mkClip (Begin 0) (Duration 0.25) ic)
    ic = curveIn instanceScale $ bounce 0.55 . smooth $ squish 1.3
    o = mkAnimation (mkClip (Begin 0) (Duration 0.25) oc)
    oc = curveIn instanceScale $ bounce 0.45 . smooth $ squish 0.9

dieAnim :: Animation Instance
dieAnim = mkAnimation . mkClip (Begin 0) (Duration 0.9) $ spin <> scale <> sink
  where
    spin = smooth . smooth . curveIn instanceRotation $ roll (V3 0 0 1)
    scale = smooth . curveIn instanceScale $ tween 1 (pure 0.05)
    sink = smooth . smooth . curveIn instancePosition $ tween 0 (V3 0 0 (-1))

attackAnim :: Instance -> Instance -> Animation Instance
attackAnim fromI toI = mkA . bounce 0.3 $ tweenInstance fromI to''
  where
    mkA = mkAnimation . mkClip (Begin 0) (Duration 0.25)
    to' = toI & instanceRotation .~ fromI ^. instanceRotation
    to'' = clipEval 0.5 (mkClip01 (tweenInstance fromI to')) fromI

jumpAnim :: Animation Instance
jumpAnim = mkAnimation . clipIn (instancePosition._z) . mkClip start len . parabola $ height
  where
    start = Begin 0
    len = Duration 0.5
    height = 2.25

moveAnim :: Instance -> Instance -> V2 Int -> V2 Int -> (Animation Instance, Instance)
moveAnim oldInst inst fromTile toTile = (recovery `after` (slide <> boing `after` windup), tI')
  where
    start = Begin 0
    len = Duration 0.7
    slide = mkAnimation . mkClip start len . backwards $ tweenInstance tI' fI'
    boing = mkAnimation . clipIn (instancePosition._z) $ mkClip start len $ parabola 3.75
    fI = inst & instancePosition._xy .~ hexToPosition fromTile
    tI = inst & instancePosition._xy .~ hexToPosition toTile
    dir = tI^.instancePosition - fI^.instancePosition
    toRot = quatBetween (V3 1 0 0) dir
    fI' = fI & instanceRotation .~ (oldInst^.instanceRotation)
    tI' = tI & instanceRotation .~ toRot
    recovery = animationIn instanceScale $ relax `after` splat
    relax = mkAnimation . mkClip start (Duration 0.2) . bounce 0.5 . smooth $ squish 1.1
    splat = mkAnimation . mkClip start (Duration 0.2) . bounce 0.5 . smooth $ squish 0.8
    windup = mkAnimation . mkClip start (Duration 0.15) . curveIn instanceScale . bounce 0.75 . smooth $ squish 1.2
