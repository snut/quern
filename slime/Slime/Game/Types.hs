
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveGeneric #-}

module Slime.Game.Types where

import Control.Concurrent.STM as STM
import Control.Concurrent (ThreadId)
import Control.Lens hiding (Level)
import Data.Aeson
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import Linear hiding (E)
import GHC.Generics (Generic)
import System.Random
import Quern.Render.Camera (Camera)
import Quern.Logging
import Quern.Util ()
import Quern.Orphans ()

newtype Entity = E Int deriving (Eq, Ord, Show, Read, ToJSON, FromJSON, FromJSONKey, ToJSONKey)

entityID :: Lens' Entity Int
entityID = lens (\(E i) -> i) (\_ i -> E i)

{-
TODO: consider an ECS or something more uniform, rather than a mess of typeclasses
-}

class HasName a where
  name :: Lens' a Text

class HasMesh a where
  mesh :: Lens' a FilePath

class HasTilePosition a where tilePosition :: Lens' a (V2 Int)
class HasTileRotation a where tileRotation :: Lens' a Float
class HasTileUniformScale a where tileScale :: Lens' a Float

data Stat = Stat
  { _statBase :: !Int
  , _statTempBonus :: !Int
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''Stat
instance ToJSON Stat
instance FromJSON Stat

statValue :: Stat -> Int
statValue (Stat b t) = b + t

-- units probably need an identifier
-- is it worth considering an ECS?
data Unit = Unit
  { _unitName :: !Text
  , _unitRotation :: !Float
  , _unitPosition :: !(V2 Int)
  , _unitScale :: !Float
  , _unitMesh :: !FilePath
  , _unitHealth :: !(V2 Stat)
  , _unitAttack :: !Stat
  , _unitCanMove :: !Bool
  , _unitCanAttack :: !Bool
  , _unitPlayerControlled :: !Bool
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''Unit
instance ToJSON Unit
instance FromJSON Unit


defaultUnit :: Unit
defaultUnit = Unit
  { _unitName = "???"
  , _unitRotation = 0
  , _unitPosition = 0
  , _unitScale = 1
  , _unitMesh = "cube"
  , _unitHealth = pure (Stat 10 0)
  , _unitAttack = Stat 2 0
  , _unitCanMove = True
  , _unitCanAttack = True
  , _unitPlayerControlled = False
  -- and so on
  }

instance HasName Unit where name = unitName
instance HasMesh Unit where mesh = unitMesh
instance HasTilePosition Unit where tilePosition = unitPosition
instance HasTileRotation Unit where tileRotation = unitRotation
instance HasTileUniformScale Unit where tileScale = unitScale

data Item = Item
  { _itemPosition :: !(V2 Int)
  , _itemRotation :: !Float
  , _itemScale :: !Float
  , _itemName :: !Text
  , _itemMesh :: !FilePath
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''Item
instance ToJSON Item
instance FromJSON Item

instance HasName Item where name = itemName
instance HasMesh Item where mesh = itemMesh
instance HasTilePosition Item where tilePosition = itemPosition
instance HasTileRotation Item where tileRotation = itemRotation
instance HasTileUniformScale Item where tileScale = itemScale

data Obstacle = Obstacle
  { _obstaclePosition :: !(V2 Int)
  , _obstacleRotation :: !Float
  , _obstacleScale :: !Float
  , _obstacleMesh :: !FilePath
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''Obstacle
instance ToJSON Obstacle
instance FromJSON Obstacle

instance HasMesh Obstacle where mesh = obstacleMesh
instance HasTilePosition Obstacle where tilePosition = obstaclePosition
instance HasTileRotation Obstacle where tileRotation = obstacleRotation
instance HasTileUniformScale Obstacle where tileScale = obstacleScale

data Level = Level
  { _levelUnits :: !(Map Entity Unit)
  , _levelItems :: !(Vector Item)
  , _levelObstacles :: !(Vector Obstacle)
  , _levelSize :: !(V2 Int)
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''Level

instance ToJSON Level
instance FromJSON Level

data Card = Card
  { _cardName :: !Text
  , _cardCost :: !Int
  , _cardIcon :: !String
  , _cardEffect :: !String
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''Card
instance ToJSON Card
instance FromJSON Card

data Player = Player
  { _playerHand :: !(Vector Card)
  , _playerDeck :: !(Vector Card)
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''Player

instance ToJSON Player
instance FromJSON Player

-- | Commands to send to the render/animation systems
data ToRender
  = Move !Entity !(V2 Int) !(V2 Int)
  | Spawn !Entity !Unit
  | Create !Item
  | Destroy !Item
  | Killed !Entity !(Maybe Entity)
  | HitWithDamage !Entity !Int !Entity
  | Missed !Entity !Entity
  | Select !(Maybe Entity)
  | ReportUnit !(Maybe (Entity, Unit))
  | DebugRandomise
  | CombatLog !String
  deriving (Eq, Ord, Show, Generic)

-- | Events from the input/window systems
data FromInput
  = Click !Camera !(V2 Int)
  | RightClick !Camera !(V2 Int)
  | Cancel
  | EndTurn
  | Quit
  | Shortcut !Char
  | LoadedLevel !Level
  | QueryUnit !Entity
  -- hmm
  | PhysDebugScreenshot !Camera !(V2 Int)
  deriving (Eq, Ord, Show, Generic)

data GameState = GameState
  { _gameLevel :: !Level
  , _gameSelectedUnit :: !(Maybe Entity)
  , _gamePlayer :: !Player
  , _gamePlayerTurn :: !Bool
  , _gameToRender :: !(TQueue ToRender)
  , _gameFromInput :: !(TQueue FromInput)
  , _gameLogHandle :: !LoggingHandle
  , _gameStdGen :: !StdGen
  }
makeLenses ''GameState

data SaveGameState = SaveGameState
  { _saveGameLevel :: !Level
  , _saveGamePlayer :: !Player
  , _saveGamePlayerTurn :: !Bool
  , _saveGameSeed :: !String
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON SaveGameState
instance FromJSON SaveGameState

instance HasLogging GameState where
  loggingHandle = gameLogHandle

data GameThread = GameThread
  { _gameThreadId :: !ThreadId
  , _gameThreadToRender :: !(TQueue ToRender)
  , _gameThreadFromInput :: !(TQueue FromInput)
  }
makeLenses ''GameThread

freshEntity :: MonadReader GameState m => m Entity
freshEntity = do
  es <- view (gameLevel . levelUnits)
  case M.maxViewWithKey es of
    Just ((E n, _), _) -> pure (E (succ n))
    Nothing -> pure (E 1)
