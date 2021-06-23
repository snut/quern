module Quern.Render
  ( module StaticScene
  , module Camera
  , module Text
  , handleViewportResize
  ) where

-- re-export rendering stuff, handy utilities
import Linear
import Graphics.GL.Core45
import Quern
import Quern.Render.Camera as Camera
import Quern.Render.Text as Text
import Quern.Render.StaticScene as StaticScene


handleViewportResize :: (HasStaticScene env, HasLogging env)
                     => Camera -> RenderText -> Quern env ()
handleViewportResize cam rt
  | not (_cameraResized cam) = pure ()
  | otherwise = do
      resizeSceneTargets (_cameraViewport cam)
      let V2 w h = fromIntegral <$> _cameraViewport cam
      glViewport 0 0 w h
      loggingString $ "Resized main window to: " <> show w <> " x " <> show h
      setRenderTextRes rt (_cameraViewport cam)
