{-# Language FlexibleContexts #-}

module Slime.Game
  ( startGameLoop
  , loadGameLoop
  , saveGame
  , ToRender(..)
  , FromInput(..)
  , Entity(..)
  , ThreadId
  , GameThread(..)
  , gameThreadId
  , gameThreadToRender
  , gameThreadFromInput
  -- * Re-exports
  , HasName
  , HasMesh
  , HasTilePosition
  , HasTileRotation
  , SaveGameState
  ) where

import Control.Concurrent.STM as STM hiding (check)
import Control.Concurrent (forkIO, ThreadId)
import Control.Lens hiding (Level)
import Control.Monad.State.Class (gets)
import Control.Monad.State (State, runState)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as B
import Data.Foldable (traverse_)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector as V
import Linear hiding (unit, trace)
import Tiles.Hex
import System.Random

import Slime.Game.Types
import Slime.Game.D20
import Quern.Types
import Quern.Physics.Primitive
--import Quern.Render.Camera (Camera(..))
import Quern.Logging

makeGameState :: MonadIO m => Level -> Int -> m GameState
makeGameState lvl seed = do
  logHdl <- startNewLogFile "game_log.txt"
  liftIO . atomically $ do
    r <- newTQueue
    i <- newTQueue
    pure $ GameState
      { _gameLevel = lvl
      , _gameSelectedUnit = Nothing
      , _gamePlayer = Player mempty mempty
      , _gamePlayerTurn = True
      , _gameToRender = r
      , _gameFromInput = i
      , _gameLogHandle = logHdl
      , _gameStdGen = mkStdGen seed
      }

loadGameState :: MonadIO m => SaveGameState -> m GameState
loadGameState save = do
  logHdl <- startNewLogFile "game_log.txt"
  liftIO . atomically $ do
    r <- newTQueue
    i <- newTQueue
    pure $ GameState
      { _gameLevel = _saveGameLevel save
      , _gameSelectedUnit = Nothing
      , _gamePlayer = _saveGamePlayer save
      , _gamePlayerTurn = _saveGamePlayerTurn save
      , _gameToRender = r
      , _gameFromInput = i
      , _gameLogHandle = logHdl
      , _gameStdGen = read (_saveGameSeed save)
      }

saveGameState ::GameState -> SaveGameState
saveGameState gs = SaveGameState
    { _saveGameLevel = _gameLevel gs
    , _saveGamePlayer = _gamePlayer gs
    , _saveGamePlayerTurn = _gamePlayerTurn gs
    , _saveGameSeed = show (_gameStdGen gs)
    }

enqueue :: (MonadIO m) => a -> TQueue a -> m ()
enqueue x q = liftIO . atomically . writeTQueue q $ x


-- Can't easily put an equivalent loadGame here, because it requires the render thread to
-- do things
saveGame :: (MonadState GameState m, MonadIO m) => FilePath -> m ()
saveGame path = do
  save <- gets saveGameState
  let bytes = encode save
  liftIO $ B.writeFile path bytes

lookupUnit :: GameState -> Entity -> Maybe (Entity, Unit)
lookupUnit gs e = (,) e <$> M.lookup e (gs ^. gameLevel . levelUnits)

lookupUnitM :: MonadState GameState m => Entity -> m (Maybe (Entity, Unit))
lookupUnitM e = do
  m <- use (gameLevel . levelUnits)
  pure $ (,) e <$> M.lookup e m

findUnitM :: MonadState GameState m => (Unit -> Bool) -> m [(Entity, Unit)]
findUnitM prd = do
    m <- use (gameLevel . levelUnits)
    pure . M.toList . M.filter prd $ m

select :: (MonadState GameState m, MonadIO m) => Maybe Entity -> m ()
select e = do
  loggingString' $ "Selected: " <> show e
  gameSelectedUnit .= e
  q <- use gameToRender
  liftIO . atomically $ writeTQueue q (Select e)

move :: (MonadState GameState m, MonadIO m) => Entity -> Unit -> V2 Int -> m ()
move e u pos
  | pos == _unitPosition u = pure ()
  | otherwise = do
      loggingString' $ "Moved: " <> unwords [show e, show pos]
      gameLevel . levelUnits . at e .= Just u{ _unitPosition = pos, _unitCanMove = False }
      use gameToRender >>= enqueue (Move e (_unitPosition u) pos)

kill :: (MonadState GameState m, MonadIO m) => Entity -> Maybe Entity -> m ()
kill e matk = do
  u <- lookupUnitM e
  case u of
    Just _ -> do
      loggingString' $ "Killed: " <> show e
      gameLevel . levelUnits . at e .= Nothing
      q <- use gameToRender
      liftIO . atomically $ writeTQueue q (Killed e matk)
    Nothing -> pure ()

withDice :: (MonadState GameState m) => State StdGen a -> m a
withDice k = do
  g <- use gameStdGen
  let (a, g') = runState k g
  gameStdGen .= g'
  pure a


attack :: (MonadState GameState m, MonadIO m) => Entity -> Entity -> m ()
attack atk dfd = do
  (hit, _) <- withDice (check Normal (DC 10) 2)
  if hit
    then damage atk dfd
    else use gameToRender >>= enqueue (Missed dfd atk)

damage :: (MonadState GameState m, MonadIO m) => Entity -> Entity -> m ()
damage atk dfd = do
  a <- lookupUnitM atk
  d <- lookupUnitM dfd
  dmg <- value <$> withDice (d4 + 1)
  case (,) <$> a <*> d of
    Nothing -> pure ()
    Just ((_,aunit), (_,dunit))
      | hlth <= dmg -> kill dfd (Just atk)
      | otherwise -> do
        gameLevel . levelUnits . at atk .= Just (aunit & unitCanAttack .~ False & unitCanMove .~ False)
        gameLevel . levelUnits . at dfd .= Just (dunit & unitHealth . _x . statBase .~ hlth - dmg)
        q <- use gameToRender
        liftIO . atomically $ do
          writeTQueue q $ HitWithDamage dfd dmg atk
          writeTQueue q $ CombatLog ("Hit for " <> show dmg)
      where
        V2 hlth _ = statValue <$> _unitHealth dunit

canMove :: Unit -> GameState -> V2 Int -> Bool
canMove u gs toTile = _unitCanMove u && distOK && destNotBlocked && destInMap
  where
    fromTile = _unitPosition u
    distOK = hexDistance fromTile toTile <= 2 -- toTile `inHex` neighbours fromTile -- >>= neighbours)
    destNotBlocked = Nothing == V.find ((== toTile).(^.tilePosition)) (gs^.gameLevel.levelObstacles)
    destInMap = all (>= 0) toTile && and ((<) <$> toTile <*> gs^.gameLevel.levelSize)

canAttack :: Unit -> Unit -> Bool
canAttack attacker defender = _unitCanAttack attacker && nearby && opposed
  where
    nearby = (_unitPosition defender) `inHex` neighbours (_unitPosition attacker)
    opposed = _unitPlayerControlled defender /= _unitPlayerControlled attacker

queryClick :: Camera -> V2 Int -> Quern (IORef GameState) (Maybe (Either (Entity,Unit) (V2 Int)))
queryClick cam cursor = do
  gs <- get
  let r = cameraPixelToRay cam cursor
      uw = makeEntityCollision gs
      tw = makeTileCollision gs
      ur = rayTest uw r
      tr = rayTest tw r
      u = nearlyToMaybe $ _resultTag <$> ur
      t = nearlyToMaybe $ _resultTag <$> tr
  case u >>= lookupUnit gs of
    Just summat -> pure $ Just (Left summat)
    _ -> case t of
      Just tile -> pure $ Just (Right tile)
      _ -> pure Nothing

dbgScreenshot :: Camera -> V2 Int -> Quern (IORef GameState) ()
dbgScreenshot cam cursor = do
  gs <- get
  let r = cameraPixelToRay cam cursor
      uw = makeEntityCollision gs
      tw = makeTileCollision gs
      w = fmap (const ()) uw <> fmap (const ()) tw
  screenshotPhysicsWorld r w "./.stuff/phys-debug"

playerLeftClick :: Camera -> V2 Int -> Quern (IORef GameState) ()
playerLeftClick cam cursor = do
  q <- queryClick cam cursor
  gs <- get
  let selected = gs^.gameSelectedUnit >>= lookupUnit gs
  case q of
    Just (Left (dfd, defender)) -> case selected of
      Just (atk, attacker)
        | _unitPlayerControlled attacker && canAttack attacker defender -> do
            attack atk dfd
            report dfd
      _ -> select (Just dfd)
    Just (Right tile) -> case selected of
      Just (selE, selU)
        | _unitPlayerControlled selU && canMove selU gs tile -> move selE selU tile
        | otherwise -> select Nothing
      _ -> pure ()
    _ -> pure ()
  reportSelected

itemsAt :: (MonadState GameState m) => V2 Int -> m (V.Vector Item)
itemsAt pos = V.filter (\i -> _itemPosition i == pos) <$> use (gameLevel . levelItems)

tileFree :: GameState -> V2 Int -> Bool
tileFree gs tile = obs && unt
  where
    obs = maybe True (const False) $
      V.find (\o -> o^.tilePosition == tile) (gs^.gameLevel.levelObstacles)
    unt = maybe True (const False) $
      V.find (\o -> o^.tilePosition == tile) . V.fromList . M.elems $  (gs^.gameLevel.levelUnits)

playerRightClick :: Camera -> V2 Int -> Quern (IORef GameState) ()
playerRightClick cam cursor = do
  q <- queryClick cam cursor
  gs <- get
  let selected = gs^.gameSelectedUnit >>= lookupUnit gs
  case q of
    Just (Right tile) -> case selected of
      Just (_, u)
        | _unitPosition u `inHex` neighbours tile && tileFree gs tile -> do
          e <- gets freshEntity
          let u' = u{ _unitPosition = tile, _unitCanMove = False, _unitCanAttack = False }
          gameLevel . levelUnits . at e .= Just u'
          use gameToRender >>= enqueue (Spawn e u')
      _ -> pure ()
    _ -> pure ()

aiMove :: (MonadIO m, MonadState GameState m) => Entity -> Unit -> m ()
aiMove e u = do
  atk <- findUnitM (\other -> canAttack u other)
  case atk of
    (dfd,_):_ -> attack e dfd
    [] -> do
      gs <- get
      let pos' = _unitPosition u - V2 0 1
      case canMove u gs pos' of
        True -> move e u pos'
        False -> pure ()

aiTurn :: (MonadIO m, MonadState GameState m) => m ()
aiTurn = do
  eus <- M.toList <$> use (gameLevel.levelUnits)
  mapM_ (uncurry aiMove) $ filter (not . _unitPlayerControlled . snd) eus
  pure ()

allUnits :: Traversal' GameState Unit
allUnits = gameLevel . levelUnits . traversed

consumeItem :: (MonadIO m, MonadState GameState m) => Entity -> Unit -> Item -> m ()
consumeItem entity unit item = do
  let unit' = unit & unitHealth . _x . statBase +~ 1
                   & unitHealth . _y . statBase +~ 1
  gameLevel . levelUnits . at entity .= Just unit'
  gameLevel . levelItems %= V.filter (/= item)
  use gameToRender >>= enqueue (Destroy item)
  --report entity

consumeOneItem :: (MonadIO m, MonadState GameState m) => Entity -> Unit -> (V.Vector Item) -> m ()
consumeOneItem entity unit items
  | V.null items = pure ()
  | otherwise = consumeItem entity unit (V.head items)

consumeItems :: (MonadIO m, MonadState GameState m) => Bool -> m ()
consumeItems playerControlled = do
    eus <- filter (\(_,u) -> _unitPlayerControlled u == playerControlled) . M.toList <$> use (gameLevel.levelUnits)
    traverse_ (uncurry consumeAt) eus
    --use gameToRender >>= enqueue (Destroy i)
    pure ()
  where
    consumeAt :: (MonadIO m, MonadState GameState m) => Entity -> Unit -> m ()
    consumeAt entity unit = itemsAt (_unitPosition unit) >>= consumeOneItem entity unit


endTurn :: (MonadIO m, MonadState GameState m) => m ()
endTurn = do
  -- consume items controlled by player
  consumeItems True
  aiTurn
  consumeItems False
  -- refresh all units
  allUnits . unitCanMove .= True
  allUnits . unitCanAttack .= True

makeEntityCollision :: GameState -> PhysicsWorld Entity
makeEntityCollision gs = PhysicsWorld $ fmap (\(e, u) -> tagShape e (shape u)) es
  where
    es = V.fromList . M.toList $ gs ^.gameLevel.levelUnits
    shape u = Sphere (V3 x y 0.25) 1 idScale where V2 x y = hexToPosition (_unitPosition u)

makeTileCollision :: GameState -> PhysicsWorld (V2 Int)
makeTileCollision gs = PhysicsWorld ts
  where
    ts = V.fromList $ hexTileRegion mkTile 0 (gs ^. gameLevel . levelSize)
    mkTile k (V2 x y) = tagShape k $ Cylinder (V3 x y (-1)) (Quaternion 1 0) (V2 1 1) idScale

startGameLoop :: MonadIO m => Level -> Int -> m GameThread
startGameLoop lvl sd = makeGameState lvl sd >>= enterGameLoop

loadGameLoop :: MonadIO m => SaveGameState -> m GameThread
loadGameLoop save = loadGameState save >>= enterGameLoop

enterGameLoop :: MonadIO m => GameState -> m GameThread
enterGameLoop gs = do
  ref <- liftIO $ newIORef gs
  gameThread <- liftIO $ forkIO (runQuern gameLoop ref)
  pure $ GameThread gameThread (_gameToRender gs) (_gameFromInput gs)

report :: (MonadIO m, MonadState GameState m) => Entity -> m ()
report e = do
  u <- lookupUnitM e
  use gameToRender >>= enqueue (ReportUnit u)

reportSelected :: (MonadIO m, MonadState GameState m) => m ()
reportSelected = do
  gs <- get
  let selected = gs^.gameSelectedUnit >>= lookupUnit gs
  use gameToRender >>= enqueue (ReportUnit selected)


gameLoop :: Quern (IORef GameState) ()
gameLoop = do
  i <- use gameFromInput
  cmd <- liftIO . atomically $ readTQueue i
  case cmd of
    Quit -> pure ()
    Click cam pixel -> playerLeftClick cam pixel *> gameLoop
    RightClick cam pixel -> playerRightClick cam pixel *> gameLoop
    Cancel -> select Nothing *> gameLoop
    EndTurn -> endTurn *> gameLoop
    LoadedLevel lvl -> (gameLevel .= lvl) *> gameLoop
    Shortcut c
      | c == 'R' || c == 'r' -> (use gameToRender >>= enqueue DebugRandomise) *> gameLoop
      | otherwise -> gameLoop
    QueryUnit e -> report e *> gameLoop
    PhysDebugScreenshot cam pixel -> dbgScreenshot cam pixel *> gameLoop
