{- -}

module Quern.Window
    ( open
    , close
    , clear
    , setClearColour
    , swap
    , shouldClose
    -- * Re-exports
    , SDL.Window
    , Inputs
    ) where

import Control.Monad.IO.Class
import Graphics.GL.Core45
import Data.Bits
import qualified Data.Text as T
import SDL.Video as SDL hiding (clear)
import SDL.Init as SDL
import SDL.Event as SDL

import SDL (($=!))
import SDL.Input.Keyboard as SDL
import SDL.Input.Keyboard.Codes as KeyCodes
import Linear


--
openGLConfig :: SDL.OpenGLConfig
openGLConfig = SDL.defaultOpenGL
  { glColorPrecision = V4 8 8 8 0
  , glDepthPrecision = 24
  , glStencilPrecision = 8
  , glMultisampleSamples = 1
  -- DEBUG: switch to SDL.Normal
  , glProfile = SDL.Core SDL.Debug 4 5
  }

wndCfg :: SDL.WindowConfig
wndCfg = defaultWindow
  { windowGraphicsContext = OpenGLContext  openGLConfig
  , windowMode = SDL.Windowed
  , windowResizable = True
  , windowInitialSize = V2 1280 720
  }

wndCfgFullscreen :: SDL.WindowConfig
wndCfgFullscreen = defaultWindow
  { windowGraphicsContext = OpenGLContext openGLConfig
  , windowMode = SDL.FullscreenDesktop
  , windowInitialSize = V2 1920 1080
  , windowBorder = False
  }

-- | Open an SDL window and initialise the SDL subsystems we need
open :: MonadIO m => T.Text -> Bool -> Maybe (Int, Int) -> m SDL.Window
open title fullscreen glVersion = do
    SDL.initialize [SDL.InitVideo, {-SDL.InitAudio,-} SDL.InitTimer, SDL.InitEvents]
    wnd <- SDL.createWindow title (versionCfg wcfg)
    ogl <- SDL.glCreateContext wnd
    SDL.glMakeCurrent wnd ogl
    configureGL
    SDL.swapInterval $=! SDL.SynchronizedUpdates
    pure $ wnd
  where
    wcfg = if fullscreen then wndCfgFullscreen else wndCfg
    versionCfg cfg = case glVersion of
      Nothing -> cfg
      -- TODO: lens this
      Just (major, minor) ->
        cfg {
          windowGraphicsContext = OpenGLContext $
            openGLConfig{
              glProfile = SDL.Core SDL.Normal (fromIntegral major) (fromIntegral minor) } }


close :: MonadIO m => SDL.Window -> m ()
close wnd = do
  SDL.destroyWindow wnd

type Inputs = [SDL.Event]

-- | Display the back buffer and grab input events
swap :: MonadIO m => SDL.Window -> m Inputs
swap window = SDL.glSwapWindow window *> SDL.pollEvents

-- grotty: should be handled as proper input loop
shouldClose :: MonadIO m => SDL.Window -> Inputs -> m Bool
shouldClose window = pure . any (isCloseEvent window)

isCloseEvent :: SDL.Window -> SDL.Event -> Bool
isCloseEvent _w (Event _time QuitEvent) = True
isCloseEvent _w (Event _time (WindowClosedEvent _closed)) = True
isCloseEvent w (Event _time (KeyboardEvent k)) = keyboardEventWindow k == Just w && keysymKeycode (keyboardEventKeysym k) == KeyCodes.KeycodeEscape
isCloseEvent _ _ = False


-- | Clear the current render target
clear :: MonadIO m => m ()
clear = glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT

-- | Set the default clear colour for the current render target
setClearColour :: MonadIO m => V4 Float -> m ()
setClearColour (V4 r g b a) = glClearColor r g b a

-- Set basic OGL state
configureGL :: MonadIO m => m ()
configureGL = do
  glEnable    GL_DEPTH_TEST
  glDepthFunc GL_GEQUAL -- default: GL_LEQUAL, reversed-z
  glDepthMask GL_TRUE
  glEnable    GL_CULL_FACE
  glCullFace  GL_BACK
  glEnable    GL_TEXTURE_CUBE_MAP_SEAMLESS
  -- GL4.5 : for reversed-z and general precision improvement
  glClipControl GL_LOWER_LEFT GL_ZERO_TO_ONE

  -- set default clear state
  glClearColor 0.125 0.125 0.125 1
  glClearDepth 0 -- default: 1, reversed-z
  clear
