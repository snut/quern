{-# Language TemplateHaskell #-}

module Quern.Render.Camera where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Linear
import SDL.Event as SDL
import SDL.Input.Keyboard as SDL
import SDL.Input.Mouse as SDL

import Quern.Input

defaultCamera :: Camera
defaultCamera = Camera
  { _cameraPosition = V3 (-3) (-3) 1
  , _cameraTarget = 0
  , _cameraUp = V3 0 0 1
  , _cameraFoV = pi/4
  , _cameraAspectRatio = 16/9
  , _cameraNear = 0.1
  , _cameraFar = Just 100
  , _cameraViewport = V2 1280 720
  , _cameraDebugMode = 0
  , _cameraDebugWire = False
  , _cameraReload = False
  , _cameraCulling = True
  , _cameraFreezeCulling = False
  , _cameraResized = False
  }

data Camera = Camera
  { _cameraPosition :: !(V3 Float)
  , _cameraTarget :: !(V3 Float)
  , _cameraUp :: !(V3 Float)
  , _cameraFoV :: !Float -- vertical fov, radians
  , _cameraAspectRatio :: !Float -- w / h
  , _cameraNear :: !Float
  , _cameraFar :: !(Maybe Float)
  , _cameraViewport :: !(V2 Int)
  , _cameraDebugMode :: !Int
  , _cameraDebugWire :: !Bool
  , _cameraReload :: !Bool -- this really should not be in the camera
  , _cameraCulling :: !Bool
  , _cameraFreezeCulling :: !Bool -- debugging cull
  , _cameraResized :: !Bool -- indicate that render targets need to be recreated
  } deriving (Eq, Ord, Show)

makeLenses ''Camera

cameraBasis :: Camera -> M33 Float
cameraBasis cam = V3 x y z
  where
    x = normalize (z `cross` _cameraUp cam)
    y = normalize (x `cross` z)
    z = normalize (_cameraTarget cam - _cameraPosition cam)

cameraView :: Camera -> M44 Float
cameraView cam = lookAt (_cameraPosition cam) (_cameraTarget cam) (_cameraUp cam)

-- regular Z, not reversed
-- reversed-z is the default now
cameraProjectionZ :: Camera -> M44 Float
cameraProjectionZ cam = case _cameraFar cam of
    Just far -> perspective (_cameraFoV cam) a near far
    Nothing -> infinitePerspective (_cameraFoV cam) a near
  where
    near = _cameraNear cam
    a = _cameraAspectRatio cam

perspectiveRevZ :: Float -> Float -> Float -> Float -> M44 Float
perspectiveRevZ fov aspect near far = m
  where
    p = tan (fov / 2)
    sx = 1 / (p * aspect)
    sy = 1 / p
    nf = negate $ near / (far - near)
    fn = (near * far) / (far - near)
    i = -1
    m = V4 (V4 sx 0  0  0)
           (V4 0  sy 0  0)
           (V4 0  0  nf fn)
           (V4 0  0  i  0)

infinitePerspectiveRevZ :: Float -> Float -> Float -> M44 Float
infinitePerspectiveRevZ fov aspect near = m
  where
    p = tan (fov / 2)
    sx = 1 / (p * aspect)
    sy = 1 / p
    i = -1
    m = V4 (V4 sx 0  0  0)
           (V4 0 sy  0  0)
           (V4 0  0  0  near)
           (V4 0  0  i  0)

-- uses reversed-z
-- https://nlguillemot.wordpress.com/2016/12/07/reversed-z-in-opengl/
cameraProjection :: Camera -> M44 Float
cameraProjection cam = case _cameraFar cam of
    Just far -> perspectiveRevZ (_cameraFoV cam) a near far
    Nothing -> infinitePerspectiveRevZ (_cameraFoV cam) a near
  where
    near = _cameraNear cam
    a = _cameraAspectRatio cam


cameraViewProjection :: Camera -> M44 Float
cameraViewProjection cam = cameraProjection cam !*! cameraView cam

cameraViewInverse :: Camera -> M44 Float
cameraViewInverse = inv44 . cameraView

cameraProjectionInverseZ :: Camera -> M44 Float
cameraProjectionInverseZ cam = case _cameraFar cam of
    Just far -> inversePerspective (_cameraFoV cam) a near far
    Nothing -> inverseInfinitePerspective (_cameraFoV cam) a near
  where
    near = _cameraNear cam
    a = _cameraAspectRatio cam

cameraProjectionInverse :: Camera -> M44 Float
cameraProjectionInverse = inv44 . cameraProjection

cameraViewProjectionInverse :: Camera -> M44 Float
cameraViewProjectionInverse cam = cameraProjectionInverse cam !*! cameraViewInverse cam

cameraForward :: Camera -> V3 Float
cameraForward cam = normalize $ _cameraTarget cam - _cameraPosition cam

cameraSideways :: Camera -> V3 Float
cameraSideways cam = normalize $ (_cameraTarget cam - _cameraPosition cam) `cross` (_cameraUp cam)

debugCamMotionMap :: M.Map Keycode (V3 Float)
debugCamMotionMap = M.fromList
  [ (KeycodeW, V3 0 1 0)
  , (KeycodeS, V3 0 (-1) 0)
  , (KeycodeA, V3 (-1) 0 0)
  , (KeycodeD, V3 1 0 0)
  , (KeycodeQ, V3 0 0 1)
  , (KeycodeE, V3 0 0 (-1))
  ]

updateCameraMotion :: Float -> Input -> Camera -> Camera
updateCameraMotion dt input cam0 = cam{ _cameraPosition = _cameraPosition cam + move, _cameraTarget = _cameraTarget cam + move }
  where
    vel = S.fold (\k v -> maybe v (+ v) (M.lookup k debugCamMotionMap)) 0 (input ^. inputKeyboard . keyboardButtons . buttonsHeld)
    V3 x y z = vel ^* (dt * spd)
    move = cameraSideways cam ^* x + cameraForward cam ^* y + _cameraUp cam ^* z
    kyb = input^.inputKeyboard.keyboardButtons
    spd = if anyButtonsHeld [KeycodeLShift, KeycodeRShift] kyb
            then 4
            else 1
    cam = camResize input $ if buttonHeld ButtonLeft (input ^. inputMouse . mouseButtons)
            then updateCameraInput input cam0
            else cam0

camResize :: Input -> Camera -> Camera
camResize input cam = cam'
  where
    sz@(V2 x y) = input ^. inputViewport . viewportSize
    cam' = cam
      { _cameraAspectRatio = fromIntegral x / fromIntegral y
      , _cameraViewport = sz
      , _cameraResized = sz /= _cameraViewport cam
      }


updateCameraInput :: Input -> Camera -> Camera
updateCameraInput input cam = updateMouseLook (input ^. inputMouse . mouseDelta) cam'
  where
    cam' =  cam{ _cameraReload = reload, _cameraResized = False }
    reload = buttonUp SDL.KeycodeR (input ^. inputKeyboard . keyboardButtons)



updateCameraMouseCapture :: MonadIO m => Input -> m LocationMode
updateCameraMouseCapture input =
  if buttonHeld ButtonLeft (input ^. inputMouse . mouseButtons)
    then SDL.setMouseLocationMode SDL.RelativeLocation
    else SDL.setMouseLocationMode SDL.AbsoluteLocation

-- based on cursor-pixel units
-- this version clamps camera pitch to prevent gimbal-lock when looking directly up and down
updateMouseLook :: V2 Int -> Camera -> Camera
updateMouseLook (V2 x y) cam = cam{ _cameraTarget = _cameraPosition cam + target' }
  where
    target = _cameraTarget cam - _cameraPosition cam
    sens = (-0.003) -- should be a setting/config thing of course
    yawDelta = sens * fromIntegral x
    pitchDelta0 = sens * fromIntegral y
    pitch0 = acos (normalize target `dot` _cameraUp cam)
    pitch1 = pitch0 + pitchDelta0
    pitchMin = 0.001
    pitchMax = pi - pitchMin
    pitchDelta = case (pitch1 <= pitchMin, pitch1 >= pitchMax) of
      (True, _) -> pitchMin - pitch0
      (_, True) -> pitchMax - pitch0
      _ -> pitchDelta0
    pitch = axisAngle (cameraSideways cam) pitchDelta
    yaw = axisAngle (_cameraUp cam) yawDelta
    target' = rotate (pitch * yaw) target



-- | Build an orthographic projection for directional lights
-- this is centred around 0 and accounts for the [0..1] clipping depth
-- implied by reversed-z (glClipControl LOWER_LEFT ZERO_TO_ONE)
--
-- usage: `orthoLight width height depth`
orthoLight :: Float -> Float -> Float -> M44 Float
orthoLight w h d =
  V4 (V4 (recip w) 0 0 0)
     (V4 0 (recip h) 0 0)
     (V4 0 0 (recip d) 1)
     (V4 0 0 0         1)
