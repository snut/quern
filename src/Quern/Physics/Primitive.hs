{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}
{-| Re-exports ray-shape tests -}

module Quern.Physics.Primitive
  ( -- * Rays
    Ray(..)
  , rayPoint
  , rayDir
  , mkRay
  , rayBetween
  , cameraPixelToRay
  , rayEval
  -- * Lines
  , Line(..)
  , lineBegin
  , lineEnd
  , lineEval
  , mkLine
  , rayToLine
  -- * Shapes
  , Shape(..)
  , TaggedShape(..)
  , taggedTag, taggedShape, taggedAABB
  , tagShape
  , idScale
  -- * Queries
  , Hit(..)
  , hitPoint, hitNormal, hitDistance
  , PhysicsWorld(..)
  , RayResult(..)
  , resultHit, resultTag
  , rayTest
  , planeTest
  , Nearly(..)
  , nearlyToMaybe
  -- * Re-exports
  , Camera
  -- * DEBUG
  , screenshotPhysicsWorld
  ) where

--import Data.Foldable (foldMap)
import Linear
import Foreign hiding (rotate)
import Quern.Render.Camera
import Quern.Physics.AxisAlignedBox
import Quern.Util ()
import Control.Lens ((^.), makeLenses)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Quern.Physics.Primitive.Types
import Quern.Physics.Primitive.RayTests
import Quern.Physics.Primitive.ShapeTests
import Quern.Physics.Primitive.ConvexHull

-- DEBUG
import Codec.Picture as Juicy
import Data.Maybe (fromJust)


cameraPixelToRay :: Camera -> V2 Int -> Ray
cameraPixelToRay cam pixel = rayBetween pos (pos + far ^* z)
  where
    pos = _cameraPosition cam
    z = fromMaybe 1e6 (_cameraFar cam)
    halfViewport = (0.5 *) . fromIntegral <$> _cameraViewport cam
    V2 u v = ((fromIntegral <$> pixel) - halfViewport) / halfViewport
    fwd = cameraForward cam
    side = cameraSideways cam
    up = fwd `cross` side
    ar = _cameraAspectRatio cam
    thX = ar * thY
    thY = tan (_cameraFoV cam * 0.5)
    far = fwd + (side ^* (u * thX)) + (up ^* (v * thY))

  -- obvious todo: hexagonal prism
  -- triangle soup?
  -- painful: convex hull


-- DEBUG
traceWorld :: PhysicsWorld a -> V3 Float -> M33 Float -> Int -> Int -> Int -> PixelRGBA8
traceWorld world eye view res x y = hitToPixel $ rayTest world ray
  where
    h = res `div` 2
    u = fromIntegral (h-x) / fromIntegral res
    v = fromIntegral (h-y) / fromIntegral res
    ray = mkRay eye (V3 u v 1 *! view)
    hitToPixel Nope = PixelRGBA8 0 0 0 0
    hitToPixel Nearly = PixelRGBA8 0 0 0 0x40
    hitToPixel (Actually (RayResult _ h)) = PixelRGBA8 r g b 0xff
      where
        V3 r g b = toByte <$> _hitNormal h
        toByte x = truncate . max 0 . min 255 $ x * 127 + 128


screenshotPhysicsWorld :: MonadIO m => Ray -> PhysicsWorld a -> FilePath -> m ()
screenshotPhysicsWorld (Ray eye dir _) world pth = do
  let img = generateImage (traceWorld world eye view res) res res
      res = 256
      view = V3 vx vy dir
      vx = normalize $ V3 0 0 1 `cross` dir
      vy = normalize $ dir `cross` vx
  liftIO $ writePng (pth <> ".png") img
