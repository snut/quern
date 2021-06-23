{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}


module Slime.Render.Types where

import Control.Lens
import Data.Aeson hiding ((.=))
import Data.IORef (IORef)
import Data.Text (Text)
--import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import Linear
import Quern
import Quern.Animation
import Quern.Input (Input)
import Quern.Render.Camera
import Quern.Render.StaticScene
import Quern.Render.Texture (TextureLibrary, HasTextureLibrary(..))
import Quern.Render.Shader (Program)
import Data.Map (Map)
import Slime.Game.Types
import Quern.Audio
import Quern.Render.Text


data RenderConfig = RenderConfig
  { _renderConfigCamOffset :: !(V3 Float)
  , _renderConfigCamFoV :: !Float -- degrees
  } deriving (Generic, Show)

instance FromJSON RenderConfig
instance ToJSON RenderConfig where
  toEncoding = genericToEncoding defaultOptions


data RenderInstance = RI !InstanceIndex !Instance
  deriving (Eq, Ord, Show)


data RenderState = RenderState
  { _camera :: !Camera
  , _input :: !Input
  , _currentFrame :: !Int
  , _lastRenderTime :: !Double
  , _animations :: !(Map Entity (Animation Instance))
  , _idleAnimations :: !(Map Entity (Animation Instance))
  , _instanceAnimations :: !(Map InstanceIndex (Animation Instance, Instance))
  , _debugCamera :: !(Maybe Camera)
  }
makeLenses ''RenderState

data GuiContext = GuiContext
  { _guiProgram :: !Program

  }
makeLenses ''GuiContext

data RenderGameData = RenderGameData
  { _entityMap :: !(Map Entity RenderInstance)
  , _itemMap :: !(Map Item RenderInstance)
  , _selectedEntity :: !(Maybe Entity)
  , _selectedUnit :: !(Maybe Unit)
  , _selectedItem :: !(Maybe Item)
  }
makeLenses ''RenderGameData

data GuiButton a = GuiButton
  { _buttonRect :: !(AABox V2 Int)
  , _buttonColour :: !(V4 Word8)
  , _buttonText :: !Text
  , _buttonAction :: !a --(Quern RenderContext ())
  }
makeLenses ''GuiButton

type ModalGui a = Input -> Quern a Bool

data RenderGui a = RenderGui
  { _guiButtons :: !(IORef [GuiButton a])
  , _renderText :: !RenderText
  , _guiModal :: !(IORef (Maybe (ModalGui a)))
  }
makeLenses ''RenderGui



data RenderContext = RenderContext
  { _window :: !Window
  , _logHandle :: !LoggingHandle
  , _scene :: !StaticScene
  , _texLib :: !TextureLibrary
  , _renderState :: !(IORef RenderState)
  , _guiContext :: !GuiContext
  , _gameThread :: !GameThread
  , _gameData :: !(IORef RenderGameData)
  , _renderStartTime :: !Double
  , _audio :: !(Maybe AudioContext)
  , _renderGui :: !(RenderGui RenderContext)
  } deriving ()
makeLenses ''RenderContext


putModalGui :: ModalGui RenderContext -> Quern RenderContext ()
putModalGui mode = mutPut (renderGui.guiModal) (Just mode)

getModalGui :: Quern RenderContext (Maybe (ModalGui RenderContext))
getModalGui = mutGet (renderGui.guiModal)

clearModalGui :: Quern RenderContext ()
clearModalGui = mutPut (renderGui.guiModal) Nothing


instance HasAudioContext RenderContext where
  audioContext = audio . _Just

instance HasLogging RenderContext where
  loggingHandle = logHandle

instance HasStaticScene RenderContext where
  staticScene = scene

instance HasTextureLibrary RenderContext where
  textureLibrary = texLib
