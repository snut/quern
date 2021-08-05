{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}
{-# Language TupleSections #-}
{-# Language RankNTypes #-}
module Main where

import Control.Concurrent.STM as STM
import Control.Lens ( view, use, (%=), (+=), (&), (.~), (^.), (.=), at, (*~), (%~))
import Control.Monad (when, forM, forM_)
import Data.Aeson hiding ((.=))
import Data.Foldable ()
import Data.IORef
import Data.List (sortOn, partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T (unpack, take)
import qualified Data.Vector  as V
--import Graphics.GL.Core45 (glClearColor)
import Linear as Linear hiding (E)
import Options.Applicative
import Quern
import Quern.Input
import Quern.Render
--import Quern.Render.Camera
--import Quern.Render.StaticScene
import Quern.Render.Texture
import Quern.Render.Shader
import SDL.Event as SDL
import SDL.Input.Keyboard.Codes
import SDL.Time as SDL
import System.Directory (doesFileExist)
import System.IO as IO
import System.IO.Error (catchIOError)
import System.Random (mkStdGen, randoms, getStdRandom, random, randomRs)
import Tiles.Hex

import Slime.Render.Types
import Slime.Game
import Slime.Game.LevelGen
import Slime.Game.Types
  ( Level(..)
  -- , Item(..)
  , HasMesh(..)
  , HasTilePosition(..)
  , HasTileRotation(..)
  , HasTileUniformScale(..)
  , Unit(..)
  , statValue )
import Slime.Render.PredefinedAnims

import Quern.Orphans ()
import Quern.Util
import Quern.Animation
import Quern.Audio
import Quern.Render.Text
import Quern.Physics.AxisAlignedBox (mkAABox)

import Quern.Render.Particles.CpuParticles (updateCpuParticleSystem, enqueuePclSpawn)

data WindowConfig = WindowConfig
  { _windowFullscreen :: !Bool
  , _windowMSAA :: !Bool
  , _windowAudio :: !Bool
  } deriving (Eq, Show)

windowConfig :: Parser WindowConfig
windowConfig = WindowConfig
  <$> switch (long "fullscreen" <> short 'f' <> help "Start in fullscreen mode")
  <*> switch (long "msaa" <> short 'm' <> help "Multisample antialiasing enabled")
  <*> switch (long "audio" <> short 'a' <> help "Audio enabled")

main :: IO ()
main = toFileHandle ".stuff/.slime.stdout" stdout . toFileHandle ".stuff/.slime.stderr" stderr $ do
  wcfg <- execParser (info (windowConfig <**> helper) (fullDesc <> progDesc "Slime!" <> header "slime - a slime game"))
  wnd <- open "Slime!" (_windowFullscreen wcfg) $ Just (4, 5)
  slib <- case _windowAudio wcfg of
    True -> Just <$> openMixer
    False -> pure Nothing
  logHdl <- startNewLogFile ".stuff/render_log.txt"
  setClearColour (V4 0.2 0.2 0.25 0)
  quernRun logHdl (renderStart wnd slib (_windowMSAA wcfg))
  close wnd
  stopLogging logHdl

renderStart :: Window -> Maybe AudioContext -> Bool -> Quern LoggingHandle ()
renderStart wnd slib msaa = do
  rseed <- liftIO (getStdRandom random)
  let lvl = generateLevel rseed --0xbeef
  state <- renderInitContext wnd slib lvl msaa
  liftIO $ quernRun state ( renderLevel lvl *> renderLoop )

frac :: RealFrac a => a -> a
frac x = x - fromIntegral (floor x :: Int)

fracMod :: RealFrac a => a -> a -> a
fracMod rng x = frac (x / rng) * rng

unitReport :: Unit -> String
unitReport u
  | _unitPlayerControlled u = unlines ["Friendly", "Move: " <> yesNo (_unitCanMove u), "Act: " <> yesNo (_unitCanAttack u), "Health: " <> show h <> " / " <> show mh]
  | otherwise = unlines ["Hostile", "Health: " <> show h <> " / " <> show mh]
  where
    V2 h mh = statValue <$> _unitHealth u
    yesNo :: Bool -> String
    yesNo True = "Yes"
    yesNo False = "No"


drawGui :: Quern RenderContext ()
drawGui = do
  rt <- view (renderGui.renderText)
  clearAllText rt
  --addString rt (V2 400 64) 0.5 "drawGui"
  sel <- magnify gameData (use selectedUnit)
  let txtStyle = defaultTextStyle{_textStyleClrFore = V4 0xe0 0xfa 0xe8 0xff, _textStyleSize = 0.75}
  case sel of
    Just u -> do
      addGuiBox rt (mkAABox (V2 20 700) (V2 324 400)) 8 2 (pure 0x80) (pure 0x20)
      addStringStyled rt (V2 24 700) txtStyle (unitReport u)

    Nothing -> pure ()

  drawDebugGui
  drawText rt

modeDesc :: Map Int String
modeDesc = M.fromList
  [ (0, "Full Shading")
  , (1, "Lit White")
  , (2, "Roughness Metallic")
  , (3, "Normal")
  , (4, "Geo Normal")
  , (5, "Tangent")
  , (6, "Bitangent")
  , (7, "Base Colour")
  , (8, "Roughness Value")
  , (9, "Diffuse Only")
  , (10, "Specular Only")
  , (11, "Ambient Occlusion")
  ]

drawDebugGui :: Quern RenderContext ()
drawDebugGui = do
  rt <- view (renderGui.renderText)
  (cam, usingDebugCamera, paused) <- magnify renderState $ do
    c <- use camera
    dc <- use debugCamera
    p <- use renderTimePaused
    pure (maybe c id dc, isJust dc, p)
  --addGuiBox :: (MonadIO m) => RenderText -> AABox V2 Int -> Int -> Int -> V4 Word8 -> V4 Word8 -> m ()

  when usingDebugCamera $ do
    let dbgMode = _cameraDebugMode cam
    addString rt (V2 24 32) 0.5 (show (_cameraPosition cam) <> "  [P]")
    addString rt (V2 24 64) 0.5 $ "Mode " <> show dbgMode <> ": " <> maybe "Unknown" id (M.lookup dbgMode modeDesc)
    addString rt (V2 24 96) 0.5 $ "F1: reset mode, F2/F3: +/- mode, F4: reload shaders"

    when paused $
      addString rt (V2 24 128) 0.5 $ "PAUSED"

    -- show 'console'
    when False $ do
      V2 w h <- subtract 16 <$> screenSize
      addGuiBox rt (mkAABox (V2 0 (h+16)) (V2 (w+32) (h - 512))) 0 8 (pure 0x20) (pure 0x10)
      hist <- logHistory
      let lgSz = 0.375
          lgLn o lgTx = do
            let V2 lnW lnH = textSize rt lgSz lgStr
                lgStr = T.unpack lgTx --(T.take 64 lgTx)
            addString rt (V2 24 (h-o)) lgSz lgStr
            pure $ o + 32
      V.foldM'_ lgLn 0 hist


-- | Main loop
renderLoop :: Quern RenderContext ()
renderLoop = do
  cam <- magnify renderState $ do
    c <- use camera
    dc <- use debugCamera
    pure $ maybe c id dc
  -- pcl stuff!
  pcls <- view (staticScene.sceneCpuParticles)
  lastDt <- magnify renderState $ do
    inp <- use input
    ldt <- use lastDeltaTime
    when (buttonHeld KeycodeL (inp^.inputKeyboard.keyboardButtons)) $
      enqueuePclSpawn pcls (V3 4 4 2, V3 1 1 3, 8)
    pure ldt
  updateCpuParticleSystem pcls lastDt (_cameraPosition cam)

  -- handle game-side stuff
  acceptGameCommands
  -- update transforms and such
  runAnims
  -- clear buffer, update cameras
  liftIO clear
  -- check for window resize
  view (renderGui.renderText) >>= handleViewportResize cam

  tD <- renderTime
  let t = realToFrac tD

  -- main scene drawing
  _ <- cullScene cam
  -- _ <- sunShadowScene t cam
  _ <- drawScene t cam

  -- draw gui
  drawGui


  w <- view window
  -- swap, handle events
  evs <- liftIO $ swap w
  ext <- liftIO $ shouldClose w evs
  case ext of
    True -> do
      q <- view (gameThread.gameThreadFromInput)
      liftIO $ atomically (writeTQueue q Quit)
    False -> do
      (dcam, dt) <- magnify renderState $ do
        updateRenderState evs
        c <- use debugCamera
        t0 <- use lastRenderTime
        lastRenderTime .= tD
        let dt = realToFrac $ tD - t0
        paused <- use renderTimePaused
        lastDeltaTime .= if paused then 0 else dt
        pure (c, dt)
      -- send relevant events to game
      case dcam of
        Just c -> updateDebugCamera c dt
        Nothing -> sendGameInputs
      renderLoop


acceptGameCommands :: Quern RenderContext ()
acceptGameCommands = do
  q <- view (gameThread.gameThreadToRender)
  cmd <- liftIO $ atomically (tryReadTQueue q)
  case cmd of
    Nothing -> pure ()
    Just c -> do
      handleCommand c
      acceptGameCommands


runAnim
  :: Lens' RenderState (Map Entity (Animation Instance))
  -> Entity
  -> Double
  -> Animation Instance
  -> RenderInstance
  -> Quern RenderContext (Maybe (Animation Instance))
runAnim anims e t a (RI idx inst) = do
  updateInstanceSlow idx $ animationEval t a inst
  let a' = maybeAnimation . fst $ pruneAnimation t a
  magnify renderState $ anims . at e .= a'
  pure a'

runAnims :: Quern RenderContext ()
runAnims = do
  t <- renderTime
  anims <- M.toList <$> magnify renderState (use animations)
  ents <- magnify gameData (use entityMap)
  forM_ anims $ \(e, a) -> case M.lookup e ents of
    Just ri -> do
      a' <- runAnim animations e t a ri
      case a' of
        Just _ -> pure ()
        Nothing -> magnify renderState (idleAnimations . at e .= Just (idleAnim t))
    Nothing -> pure ()
  idles <- M.toList <$> magnify renderState (use idleAnimations)
  forM_ idles $ \(e,a) -> case M.lookup e ents of
    Just ri -> do
      a' <- runAnim idleAnimations e t a ri
      case a' of
        Just _ -> pure ()
        Nothing -> magnify renderState (idleAnimations . at e .= Just (idleAnim t))
    Nothing -> pure ()

  ianims <- M.toList <$> magnify renderState (use instanceAnimations)
  forM_ ianims $ \(idx, (a, inst)) -> do
    updateInstanceSlow idx $ animationEval t a inst
    let a' = fst $ pruneAnimation t a
    magnify renderState $ instanceAnimations . at idx .= ((,inst) <$> maybeAnimation a')

addAnim :: Entity -> Animation Instance -> Quern RenderContext ()
addAnim e a0 = do
  t <- renderTime
  magnify renderState $ do
    let a' = a0 & beginTime .~ t
    anims <- use animations
    case M.lookup e anims of
      Just a ->
        animations . at e .= Just (a' `after` a)
      Nothing ->
        animations . at e .= Just a'
    idleAnimations . at e .= Nothing


addStaticAnim :: InstanceIndex -> Instance -> Animation Instance -> Quern RenderContext ()
addStaticAnim idx inst anim = do
  t <- renderTime
  magnify renderState $ do
    let a' = anim & beginTime .~ t
    anims <- use instanceAnimations
    case M.lookup idx anims of
      Just (a, inst0) -> instanceAnimations . at idx .= Just (a' `after` a, inst0)
      Nothing -> instanceAnimations . at idx .= Just (a', inst)

clearAllAnimations :: Quern RenderContext ()
clearAllAnimations = magnify renderState $ do
    instanceAnimations .= mempty
    animations .= mempty

enqueueGameThread :: FromInput -> Quern RenderContext ()
enqueueGameThread x = do
  q <- view (gameThread.gameThreadFromInput)
  liftIO $ atomically (writeTQueue q x)

enqueuePclSpawn' :: (V3 Float, V3 Float, Int) -> Quern RenderContext ()
enqueuePclSpawn' sp = do
  pcls <- view (scene.sceneCpuParticles)
  enqueuePclSpawn pcls sp

handleCommand :: ToRender -> Quern RenderContext ()
handleCommand (Move e fromTile toTile) = do
  ents <- _entityMap <$> mutGet gameData
  case M.lookup e ents of
    Just (RI idx inst) -> do
      let inst' = inst & instancePosition._xy .~ hexToPosition toTile
          (anim, inst'') = moveAnim inst inst' fromTile toTile
          ri = RI idx inst''
      addAnim e anim
      magnify gameData $ entityMap .= M.insert e ri ents
    Nothing -> pure ()
handleCommand (Spawn e u) = do
  ri <- drawThing u
  playOnce "bells" 0
  magnify gameData $ entityMap . at e .= Just ri
  addAnim e spawnAnim
handleCommand (Create i) = do
  ri <- drawThing i
  magnify gameData $ itemMap . at i .= Just ri
  pure ()
handleCommand (Destroy i) = do
  itm <- magnify gameData (use (itemMap . at i))
  case itm of
    Just (RI idx inst) -> do
      addStaticAnim idx inst dieAnim
      magnify gameData (itemMap . at i .= Nothing)
    Nothing -> pure ()
handleCommand (Killed e atker) = do
  m <- magnify gameData (use (entityMap . at e))
  case m of
    Just (RI idx inst) -> do
      case atker of
        Just atk -> do
          matk' <- magnify gameData (use (entityMap . at atk))
          case matk' of
            Just (RI _ atInst) -> addAnim atk (attackAnim atInst inst)
            Nothing -> pure ()
        Nothing -> pure ()
      enqueuePclSpawn' (inst^.instancePosition + V3 0 0 1, V3 0 0 1, 32)
      a <- magnify renderState $ do
        an <- use (animations . at e)
        animations . at e .= Nothing
        pure an
      case a of
        Just anm -> addStaticAnim idx inst anm
        Nothing -> pure ()
      addStaticAnim idx inst (dieAnim `after` idAnimation (0.3 * 0.25))
      magnify gameData (entityMap . at e .= Nothing)
    Nothing -> pure ()
handleCommand (HitWithDamage e _ atk) = do
  ents <- _entityMap <$> mutGet gameData
  case M.lookup e ents of
    Just (RI _ to) -> do

      case M.lookup atk ents of
        Just (RI _ from) -> do
          enqueuePclSpawn' (to^.instancePosition + V3 0 0 1, V3 0 0 1.5 + to^.instancePosition - from^.instancePosition, 64)
          addAnim atk (attackAnim from to)
        _ -> enqueuePclSpawn' (to^.instancePosition + V3 0 0 1, 0, 32)
    _ -> pure ()
  addAnim e (squidge `after` idAnimation (0.3 * 0.25))
handleCommand (Missed dfd atk) = do
  ents <- _entityMap <$> mutGet gameData
  case M.lookup dfd ents of
    Just (RI _ to) ->
      case M.lookup atk ents of
        Just (RI _ from) -> do
          addAnim atk (attackAnim from to)
          addAnim dfd jumpAnim
        _ -> pure ()
    _ -> pure ()
handleCommand (Select mbE) = do
  oldE <- magnify gameData (use selectedEntity)
  case mbE of
    Just e -> do
      case oldE of
        Just o
          | o == e -> pure ()
          | otherwise -> do
              addAnim e squidge
              enqueueGameThread (QueryUnit e)
              --magnify gameData (selectedEntity .= Just e)
        Nothing -> do
          addAnim e squidge
          enqueueGameThread (QueryUnit e)
          --magnify gameData (selectedEntity .= Just e)
    Nothing -> do
      magnify gameData $ do
        selectedEntity .= Nothing
        selectedUnit .= Nothing
handleCommand (ReportUnit Nothing) = magnify gameData $ do
  selectedUnit .= Nothing
  selectedEntity .= Nothing
handleCommand (ReportUnit (Just (e, u))) = magnify gameData $ do
  selectedUnit .= Just u
  selectedEntity .= Just e
handleCommand (CombatLog _msg) = pure ()
handleCommand (DebugRandomise) = do
  clearAllAnimations
  clearAllInstances
  rseed <- liftIO (getStdRandom random)
  let lvl = generateLevel rseed
  renderLevel lvl
  enqueueGameThread (LoadedLevel lvl)

guiClicking :: V2 Int -> Bool -> Quern RenderContext Bool
guiClicking _pos _leftClick = pure False

sendGameInputs :: Quern RenderContext ()
sendGameInputs = do
    rs <- mutGet renderState
    let c = rs ^. camera
        i = rs ^. input
    q <- view (gameThread.gameThreadFromInput)
    let lclicked = buttonUp ButtonLeft (i ^. inputMouse . mouseButtons)
        rclicked = buttonUp ButtonRight (i ^. inputMouse . mouseButtons)
        kb = i ^. inputKeyboard . keyboardButtons
        enterClicked = buttonUp KeycodeReturn kb
        rChar = buttonUp KeycodeR kb
        pos = i ^. inputMouse . mousePosition
        physDebug = buttonUp KeycodeF12 kb
    -- hmm
    handledByGui <- guiClicking pos lclicked
    when (not handledByGui) $ do
      when lclicked $
        liftIO $ atomically (writeTQueue q (Click c pos))
      when physDebug $
        liftIO $ atomically (writeTQueue q (PhysDebugScreenshot c pos))
      when rclicked $
        liftIO $ atomically (writeTQueue q (RightClick c pos))
      when enterClicked $
        liftIO $ atomically (writeTQueue q EndTurn)
      when rChar $
        liftIO $ atomically (writeTQueue q (Shortcut 'r'))

screenSize :: Quern RenderContext (V2 Int)
screenSize = do
  rs <- mutGet renderState
  pure $ rs^.camera.cameraViewport

debugVisModes :: Input -> Camera -> Camera
debugVisModes ip cam
  | buttonUp KeycodeF1 kb = cam & cameraDebugMode .~ 0
  | buttonUp KeycodeF2 kb = cam & cameraDebugMode %~ ((`mod` modes) . succ)
  | buttonUp KeycodeF3 kb = cam & cameraDebugMode %~ ((`mod` modes) . (+ pred modes))
  | otherwise = cam
  where
    kb = ip^.inputKeyboard.keyboardButtons
    modes = 12

updateDebugCamera :: Camera -> Float -> Quern RenderContext ()
updateDebugCamera c dt = do
  rs <- mutGet renderState
  _ <- updateCameraMouseCapture (rs^.input)
  let c' = updateCameraMotion dt (rs^.input) (debugVisModes (rs^.input) c)
  when (buttonUp KeycodeF4 (rs^.input.inputKeyboard.keyboardButtons))
    (view scene >>= reloadScenePrograms)
  magnify renderState $ debugCamera .= Just c'

updateRenderState :: [SDL.Event] -> Quern (IORef RenderState) ()
updateRenderState evs = do
  cam <- use camera
  dbgCam <- use debugCamera
  input %= updateInput evs
  inp <- use input
  let cam' = camResize inp cam
  camera .= cam'

  dbg <- if buttonUp KeycodeP (inp^.inputKeyboard.keyboardButtons)
    then case dbgCam of
      Just _ -> (debugCamera .= Nothing) *> pure False
      Nothing -> (debugCamera .= Just cam') *> pure True
    else case dbgCam of
      Just c -> when (cam'^.cameraResized) (debugCamera .= Just (camResize inp c)) *> pure True
      Nothing -> pure False

  pause <- use renderTimePaused
  when (dbg && buttonUp KeycodeK (inp^.inputKeyboard.keyboardButtons)) $ do
    renderTimePaused .= not pause

  when (not dbg && pause) (renderTimePaused .= False)

  currentFrame += 1


initCam :: RenderConfig -> V2 Int -> Camera
initCam cfg p =
    let V2 tx ty = hexToPosition p
    in defaultCamera
      { _cameraPosition = V3 tx ty 0 + (_renderConfigCamOffset cfg)
      , _cameraTarget = V3 tx ty 0
      , _cameraFoV = pi * (_renderConfigCamFoV cfg) / 180
      }

initCfg :: RenderConfig
initCfg = RenderConfig (V3 0 (-16) 6) 27

-- | Loads the 'global' things required for a render context, requiring only a logging handle
renderInitContext :: Window -> Maybe AudioContext -> Level -> Bool -> Quern LoggingHandle RenderContext
renderInitContext window0 slib lvl msaa = do
  logHdl <- ask
  t0 <- SDL.time
  let ms = if msaa then Ms2x else MsNone
      initRes = V2 1280 720
  scene0 <- newStaticScene "data/environments/misty_pines_1k.hdr" initRes ms
  texLib0 <- newTextureLibrary
  input0 <- initInput (Just window0)
  cfg <- liftIO $ catchIOError (decodeFileStrict "./data/render_config.json") (\_ -> pure Nothing)
  let cam = initCam (fromMaybe initCfg cfg) (V2 4 3)
  st <- liftIO $ newIORef $ RenderState
                              { _camera = cam
                              , _input = input0
                              , _currentFrame = 0
                              , _lastRenderTime = t0
                              , _lastDeltaTime = 0
                              , _animations = mempty
                              , _idleAnimations = mempty
                              , _instanceAnimations = mempty
                              , _debugCamera = Nothing
                              , _renderTimePaused = False
                              }
  ents <- liftIO $ newIORef (RenderGameData mempty mempty Nothing Nothing Nothing)
  guiProg <- either error id <$> loadKernelFromFile "data/shaders/sdf2d.comp"
  gthread <- startGameLoop lvl 0xbeef
  let gui = GuiContext guiProg

  rtext <- newRenderText "data/fonts/open_sans.fnt" initRes
  rgui <- liftIO $ RenderGui <$> newIORef [] <*> pure rtext <*> newIORef Nothing
  -- hmm, getting a bit cumbersome
  pure $ RenderContext
    { _window = window0
    , _logHandle = logHdl
    , _scene = scene0
    , _texLib = texLib0
    , _renderState = st
    , _guiContext = gui
    , _gameThread = gthread
    , _gameData = ents
    , _renderStartTime = t0
    , _audio = slib
    , _renderGui = rgui
    }

renderTime :: Quern RenderContext Double
renderTime = do
  t1 <- SDL.time
  t0 <- view renderStartTime
  pure $ t1 - t0

-- hex tile and rotation [0..6] -> instance
hexInstance :: Float -> V2 Int -> Float -> Instance
hexInstance d p s = mkInstance r (V3 x y 0) (pure s)
  where
    V2 x y = hexToPosition p
    r = axisAngle (V3 0 0 1) (d * pi / 3)

-- draw an instance of a mesh
drawOne :: FilePath -> Instance -> Quern RenderContext InstanceIndex
drawOne msh inst = do
  is <- drawSome msh [inst]
  case is of
    [i] -> pure i
    _ -> error "Impossible"

-- draw some instances of a mesh
drawSome :: FilePath -> [Instance] -> Quern RenderContext [InstanceIndex]
drawSome _ [] = pure []
drawSome msh insts = do
  let meshPath = "data/meshes/" <> msh
  meshM <- loadMesh meshPath
  fmap fst <$> traverse (drawInstanceDflt meshM) insts


drawSome_ :: FilePath -> [Instance] -> Quern RenderContext ()
drawSome_ msh insts = drawSome msh insts *> pure ()

-- add a thing to the scene based on a tile position and rotation
drawThing :: (HasMesh a, HasTilePosition a, HasTileRotation a, HasTileUniformScale a)
          => a -> Quern RenderContext RenderInstance
drawThing a = RI <$> drawOne (a^.mesh) inst <*> pure inst
  where inst = hexInstance (a^.tileRotation) (a^.tilePosition) (a^.tileScale)

-- add a thing to the static scene, with a spawn animation
spawnThing :: (HasMesh a, HasTilePosition a, HasTileRotation a, HasTileUniformScale a)
           => a -> Quern RenderContext RenderInstance
spawnThing a = do
  ri@(RI ix inst) <- drawThing a
  let s = 0.15 * norm (inst ^. instancePosition)
  dspawn <- delaySpawn (realToFrac s) 1
  addStaticAnim ix inst dspawn
  pure ri

renderLevel :: Level -> Quern RenderContext ()
renderLevel lvl = do
  dfltMtl <- simpleMaterial "*default" (V3 0.8 0.8 0.8) 0.75 False
  loggingString $ "*default: " <> show dfltMtl
  let g = mkStdGen 32
      rots  = ZipList $ fmap (fromIntegral . (`mod` 5)) (randoms g :: [Int])
      posns = ZipList $ hexTileRegion const (pure (-1)) (_levelSize lvl)
      scales = ZipList $ randomRs (1.0, 1.1) (mkStdGen 33)
      tilt = 0.05
      tiltX = ZipList $ fmap (axisAngle (V3 1 0 0)) (randomRs (negate tilt, tilt) (mkStdGen 34))
      tiltY = ZipList $ fmap (axisAngle (V3 0 1 0)) (randomRs (negate tilt, tilt) (mkStdGen 35))

      hexNoTilt = hexInstance <$> rots <*> posns <*> scales

      hexes = getZipList $ (\i x y -> i & instanceRotation *~ (x*y)) <$> hexNoTilt <*> tiltX <*> tiltY
      (dryHexes, greenHexes) = partition (\t -> norm (t^.instancePosition) < 5) hexes
  drawSome_ "tiles/hex_tile" greenHexes
  drawSome_ "tiles/clutter_hex" dryHexes

  -- add some big background stumps
  drawOne "props/stump" $ mkInstance (axisAngle (V3 0 1 0) (pi*(-0.1))) (V3 18 17 (-6)) (pure 8)
  drawOne "props/stump" $ mkInstance (axisAngle (V3 0 0 1) (pi*0.9)) (V3 (-3) 19 (-6)) (pure 10)

  -- movement blockers
  V.mapM_ spawnThing (_levelObstacles lvl)

  -- items
  let startItems = _levelItems lvl
  itemInsts <- V.mapM spawnThing startItems

  -- Slimes!
  mp <- forM (M.toList (_levelUnits lvl)) $ \(e, u) -> do
    ri <- drawThing u
    addAnim e spawnAnim
    loggingString $ show e <> " " <> show (hexToPosition (u^.tilePosition))
    pure (e, ri)

  magnify gameData $ do
    itemMap .= M.fromList (V.toList (V.zip startItems itemInsts))
    entityMap .= M.fromList mp
  meshes <- view (scene.sceneMeshes)
  meshStorageReport meshes >>= mapM_ (loggingString . show) . sortOn snd

  -- startMusic "./data/music/some_music.mp3"
  pure ()
