{-# Language TemplateHaskell #-}
module Quern.Input
  ( -- * Types and lenses
    Buttons
  , buttonsDown, buttonsUp, buttonsHeld
  , Mouse
  , mousePosition, mouseDelta, mouseButtons
  , Keyboard
  , keyboardButtons
  , Viewport
  , viewportSize, viewportFocus, viewportMinimised
  , Input
  , inputMouse, inputKeyboard, inputViewport
  -- * Initialisation and update
  , initInput
  , updateInput
  , buttonHeld
  , anyButtonsHeld
  , buttonUp
  , buttonDown
  , consumeButton
  -- * Re-exports
  , SDL.MouseButton(..)
  , SDL.Keycode(..)
  -- unsure about this one
  -- , module SDL.Input.Keyboard.Codes
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable (foldl')
import Data.Set (Set, member, insert, delete)
import qualified Data.Set as S
import Data.StateVar (get)
import Linear
import Linear.Affine (Point(..))
import SDL.Event as SDL
import SDL.Input.Keyboard as SDL
import SDL.Input.Mouse as SDL hiding (Mouse)
import SDL.Video as SDL

data Buttons a = Buttons
  { _buttonsDown :: !(Set a)
  , _buttonsUp :: !(Set a)
  , _buttonsHeld :: !(Set a)
  } deriving (Show, Read)
makeLenses ''Buttons

data Mouse = Mouse
  { _mousePosition :: !(V2 Int)
  , _mouseDelta :: !(V2 Int)
  , _mouseButtons :: !(Buttons SDL.MouseButton)
  } deriving (Show, Read)
makeLenses ''Mouse

newtype Keyboard = Keyboard
  { _keyboardButtons :: (Buttons SDL.Keycode)
  } deriving (Show, Read)
makeLenses ''Keyboard

data Viewport = Viewport
  { _viewportSize :: !(V2 Int)
  , _viewportFocus :: !Bool
  , _viewportMinimised :: !Bool
  } deriving (Show, Read)
makeLenses ''Viewport

data Input = Input
  { _inputMouse :: !Mouse
  , _inputKeyboard :: !Keyboard
  -- technically should be associated with a window
  -- Map (Maybe Window) Viewport ?
  , _inputViewport :: !Viewport
  } deriving (Show, Read)
makeLenses ''Input

initInput :: MonadIO m => Maybe SDL.Window -> m Input
initInput windowMay = case windowMay of
  Just window -> do
    size <- fmap fromIntegral <$> get (windowSize window)
    (P mpos) <- getAbsoluteMouseLocation
    pure $ Input (Mouse (fromIntegral <$> mpos) 0 mempty) (Keyboard mempty) (Viewport size True False)
  Nothing ->
    pure $ Input (Mouse 0 0 mempty) (Keyboard mempty) (Viewport 100 True False)

updateInput :: [SDL.Event] -> Input -> Input
updateInput events input = foldl' go input0 (eventPayload <$> events)
  where
    go i (KeyboardEvent kb) = i & inputKeyboard %~ handleKB kb
    go i (MouseMotionEvent ms) = i & inputMouse %~ handleMouseMove ms
    go i (MouseButtonEvent mb) = i & inputMouse %~ handleMouseButton mb
    go i (WindowLostMouseFocusEvent _) = clearButtons i & inputViewport . viewportFocus .~ False
    go i (WindowGainedMouseFocusEvent _) = i & inputViewport . viewportFocus .~ True
    go i (WindowMinimizedEvent _) = clearButtons i & inputViewport . viewportMinimised .~ True
    go i (WindowMaximizedEvent _) = i & inputViewport . viewportMinimised .~ False
    go i (WindowRestoredEvent _) = i & inputViewport . viewportMinimised .~ False
    go i (WindowSizeChangedEvent (WindowSizeChangedEventData _window size)) = clearButtons i & inputViewport . viewportSize .~ (fromIntegral <$> size)
    go i _ = i
    input0 = input
      & inputMouse . mouseDelta .~ 0
      & inputMouse . mouseButtons . buttonsDown .~ mempty
      & inputMouse . mouseButtons . buttonsUp .~ mempty
      & inputKeyboard . keyboardButtons . buttonsDown .~ mempty
      & inputKeyboard . keyboardButtons . buttonsUp .~ mempty

handleKB :: KeyboardEventData -> Keyboard -> Keyboard
handleKB (KeyboardEventData _windowMay motion _isRepeat keysym) kb = case motion of
    SDL.Pressed  -> kb & keyboardButtons %~ buttonPress code
    SDL.Released -> kb & keyboardButtons %~ buttonRelease code
  where
    code = keysymKeycode keysym

handleMouseMove :: MouseMotionEventData -> Mouse -> Mouse
handleMouseMove (MouseMotionEventData _windowMay _device _buttons (P pos) delta) mouse =
  mouse & mouseDelta .~ (fromIntegral <$> delta)
        & mousePosition .~ (fromIntegral <$> pos)

handleMouseButton :: MouseButtonEventData -> Mouse -> Mouse
handleMouseButton (MouseButtonEventData _windowMay motion _device button _clicks _pos) mouse = case motion of
  SDL.Pressed -> mouse & mouseButtons %~ buttonPress button
  SDL.Released -> mouse & mouseButtons %~ buttonRelease button

clearButtons :: Input -> Input
clearButtons input = input
  & inputMouse . mouseButtons .~ mempty
  & inputKeyboard . keyboardButtons .~ mempty

instance Ord a => Semigroup (Buttons a) where
  (Buttons d0 u0 h0) <> (Buttons d1 u1 h1) = Buttons (d0 <> d1) (u0 <> u1) (h0 <> h1)

instance Ord a => Monoid (Buttons a) where
  mempty = Buttons mempty mempty mempty

buttonHeld :: Ord a => a -> Buttons a -> Bool
buttonHeld button = member button . _buttonsHeld

anyButtonsHeld :: Ord a => [a] -> Buttons a -> Bool
anyButtonsHeld buttons = not . S.null . S.intersection (S.fromList buttons) . _buttonsHeld

buttonUp :: Ord a => a -> Buttons a -> Bool
buttonUp button = member button . _buttonsUp

buttonDown :: Ord a => a -> Buttons a -> Bool
buttonDown button = member button . _buttonsDown


buttonPress :: Ord a => a -> Buttons a -> Buttons a
buttonPress button buttons =
  buttons & buttonsDown %~ insert button
          & buttonsHeld %~ insert button

buttonRelease :: Ord a => a -> Buttons a -> Buttons a
buttonRelease button buttons =
  buttons & buttonsUp %~ insert button
          & buttonsHeld %~ delete button

consumeButton :: Ord a => a -> Buttons a -> Buttons a
consumeButton button buttons =
    buttons & buttonsUp %~ delete button
            & buttonsDown %~ delete button
            & buttonsHeld %~ delete button
