{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts, RankNTypes #-}
{-# Language OverloadedStrings #-}

{-| A module for rendering particles that are simulated in Haskell code
and then pushed to the GPU for rendering only -}
module Quern.Render.Particles.CpuParticles where

import System.Random
import Control.Monad.State
import Quern.Physics.AxisAlignedBox
import Quern.Render.Internal.StorageBuffer
import Quern.Logging
import Graphics.GL.Core45
import Linear
import Quern.Util
import Control.Monad.ST
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign hiding (rotate)
import Data.Foldable (traverse_, foldlM)
import Quern.Render.Shader
import Quern.Render.Texture
import Data.IORef
import Control.Monad.Primitive (PrimState)
import Control.Lens (makeLenses, (^.), (*~), (&))
import Control.Concurrent.STM
import qualified Data.Vector.Algorithms.Tim as VA


-- Generating useful values for particles

-- | 1-dimensional bounding boxes, or intervals as sensible people might call them
type DistRange a = AABox V1 a

mkRange :: Ord a => a -> a -> DistRange a
mkRange a b = mkAABox (V1 a) (V1 b)

-- Don't have uniformR in this version of System.Random
-- | Generate a value that lies within an interval
uniformR :: (Random a, RandomGen g) => DistRange a -> g -> (a, g)
uniformR r = randomR (mn, mx)
  where
    V1 mn = aaBoxMin r
    V1 mx = aaBoxMax r

canonicalR :: (Ord a, Num a) => DistRange a
canonicalR = mkRange 0 1

angularR :: (Ord a, Floating a) => DistRange a
angularR = mkRange (-pi) pi


data V3Dist
  = SphereDist !(V3 Float) !(DistRange Float)
  | ConeDist !(V3 Float) !(DistRange Float) !(Quaternion Float) !(DistRange Float)
  deriving (Eq, Ord, Show, Read)

-- | This might be better expressed as just passing functions around rather than
-- using a data type. Serialisation is the concern.
distV3 :: MonadState StdGen m => V3Dist -> m (V3 Float)
distV3 (SphereDist centre radii) = do
  r <- state $ uniformR radii
  u <- state $ uniformR canonicalR
  v <- state $ uniformR canonicalR
  pure $! centre + r *^ canonicalToUnit u v
distV3 (ConeDist centre radii rot angles) = do
  r <- state $ uniformR radii
  t <- state $ uniformR angles
  p <- state $ uniformR angularR
  let v = V3 (cos p * u) (sin p * u) z
      z = cos t
      u = sqrt $ 1 - z*z
  pure $! centre + r *^ rotate rot v

-- This is a simulated particle
data Particle = Pcl
  { _pclPos :: !(V3 Float)
  , _pclVel :: !(V3 Float)
  , _pclAngVel :: !(V3 Float)
  , _pclSize :: !(V3 Float)
  , _pclRot :: !(Quaternion Float)
  , _pclTtl :: !Float
  , _pclSeed :: !Word32
  , _pclLifespan :: !Float
  } deriving (Eq, Ord, Show)

makeLenses ''Particle

instance Storable Particle where
  sizeOf _ = 12 * 4 + 16 + 4 + 4 + 4
  alignment _ = alignment (0 :: Float)
  peek ptr = Pcl
    <$> peek (castPtr ptr) -- pos
    <*> peek (plusPtr ptr 12) -- vel
    <*> peek (plusPtr ptr 24) -- ang. vel
    <*> peek (plusPtr ptr 36) -- size
    <*> peek (plusPtr ptr 48) -- rot
    <*> peek (plusPtr ptr 64) -- ttl
    <*> peek (plusPtr ptr 68) -- seed
    <*> peek (plusPtr ptr 72) -- lifespan
  poke ptr pcl = do
    poke (castPtr ptr) (_pclPos pcl)
    poke (plusPtr ptr 12) (_pclVel pcl)
    poke (plusPtr ptr 24) (_pclAngVel pcl)
    poke (plusPtr ptr 36) (_pclSize pcl)
    poke (plusPtr ptr 48) (_pclRot pcl)
    poke (plusPtr ptr 64) (_pclTtl pcl)
    poke (plusPtr ptr 68) (_pclSeed pcl)
    poke (plusPtr ptr 72) (_pclLifespan pcl)

-- evolving particles
updateParticle :: Float -> Particle -> Particle
updateParticle 0  pcl = pcl
updateParticle dt pcl = if pcl'^.pclPos._z < 0 then pcl'&pclVel *~ bounce else pcl'
  where
    bounce = V3 0.9 0.9 (-0.7)
    pcl' = pcl
      { _pclPos = _pclPos pcl + _pclVel pcl ^* dt
      , _pclVel = _pclVel pcl - (V3 0 0 (9.8*dt))
      , _pclRot = _pclRot pcl * (axisAngle (_pclAngVel pcl) (norm (_pclAngVel pcl) * dt))
      , _pclTtl = _pclTtl pcl - dt
      }

-- spawning particles
spawnParticle :: MonadState StdGen m => V3Dist -> V3Dist -> Float -> m Particle
spawnParticle pos vel dt = do
  p <- distV3 pos
  v <- distV3 vel
  ang <- state $ uniformR angularR
  sd <- state random
  let lifespan = 4
  let pcl0 = Pcl
              { _pclPos = p
              , _pclVel = v
              , _pclAngVel = 0
              , _pclSize = 0.125
              , _pclRot = axisAngle (V3 0 0 1) ang
              , _pclTtl = lifespan
              , _pclSeed = sd
              , _pclLifespan = lifespan
              }
  pure $ updateParticle dt pcl0

spawnParticles :: StdGen -> V3Dist -> V3Dist -> Int -> Float -> (VS.Vector Particle, StdGen)
spawnParticles gen pos dir count dt = runST $ runStateT body gen
  where
    rcpCount = recip $ fromIntegral count
    body :: forall s. StateT StdGen (ST s) (VS.Vector Particle)
    body = do
      vs <- VSM.new count
      flip traverse_ [0..count-1] $ \i -> do
        let u = fromIntegral i * rcpCount
        pcl <- spawnParticle pos dir (dt*u)
        VSM.unsafeWrite vs i pcl
      VS.unsafeFreeze vs

-- | Data type used for rendering particles on the GPU
-- most of this can be half precision
data PclInstance = PclInstance
  { _pclInstPos :: !(V3 Float)
  , _pclInstAge :: !Float
  , _pclInstSize :: !(V3 Float)
  , _pclInstFade :: !Float
  , _pclInstRot :: !(Quaternion Float)
  -- packed
  , _pclInstSeed :: !Word32
  , _pclInstClr :: !(V4 Word8)
  , _pclInstBright :: !Float
  , _pclInstLifespan :: !Float
  }

instance Storable PclInstance where
  sizeOf _ = sizeOf (0 :: Float) * (4+4+4+4)
  alignment _ = alignment (undefined :: V3 Float)
  peek ptr = PclInstance
    <$> peek (castPtr ptr)
    <*> peek (plusPtr ptr 12)
    <*> peek (plusPtr ptr 16)
    <*> peek (plusPtr ptr 28)
    <*> peek (plusPtr ptr 32)
    -- individual properties
    <*> peek (plusPtr ptr 48)
    <*> peek (plusPtr ptr 52)
    <*> peek (plusPtr ptr 56)
    <*> peek (plusPtr ptr 60)

  poke ptr (PclInstance pos age sz fade rot seed clr bright life) = do
    poke (castPtr ptr) pos
    poke (plusPtr ptr 12) age
    poke (plusPtr ptr 16) sz
    poke (plusPtr ptr 28) fade
    poke (plusPtr ptr 32) rot
    poke (plusPtr ptr 48) seed
    poke (plusPtr ptr 52) clr
    poke (plusPtr ptr 56) bright
    poke (plusPtr ptr 60) life


pclInstance :: Particle -> PclInstance
pclInstance pcl = PclInstance (_pclPos pcl) (_pclTtl pcl) (_pclSize pcl) 0.1 (_pclRot pcl) (_pclSeed pcl) clr bright (_pclLifespan pcl)
  where
    clr = pure 0xff
    bright = 1.0

type PclSpawnParams = (V3 Float, V3 Float, Int)

data CpuParticleSystem = CpuParticleSystem
  { _cpuParticleVertices :: !(StorageBuffer (V3 Float))
  , _cpuParticleIndices :: !(StorageBuffer Word16)
  , _cpuParticleInstances :: !(StorageBuffer PclInstance)
  , _cpuParticleVao :: !GLuint
  , _cpuParticleProgram :: !Program
  , _cpuParticleCount :: !(IORef Int)
  , _cpuParticleBuffer :: !(VSM.MVector (PrimState IO) Particle) --
  , _cpuParticleInstanceBuffer :: !(VSM.MVector (PrimState IO) PclInstance) -- VSM.unsafeWith allows using this for buffer writes
  , _cpuParticleSpawning :: !(TQueue PclSpawnParams)
  , _cpuParticleTexture :: !Texture -- a simple test atlas
  }

enqueuePclSpawn :: MonadIO m => CpuParticleSystem -> PclSpawnParams -> m ()
enqueuePclSpawn pcls = liftIO . atomically . writeTQueue (_cpuParticleSpawning pcls)

debugPclSpawn :: MonadIO m => PclSpawnParams -> m (VS.Vector Particle)
debugPclSpawn (pos,vel, count) = do
  debugGen <- liftIO $ getStdGen
  let (debugPcls,debugGen') = spawnParticles debugGen (SphereDist pos (mkRange 0.125 0.5)) (SphereDist vel (mkRange 0.5 1)) count (1/60)
  liftIO $ setStdGen debugGen'
  pure debugPcls

cpuParticleMax :: Int
cpuParticleMax = 65536

newCpuParticleSystem :: (MonadReader env m, HasLogging env, MonadIO m) => m CpuParticleSystem
newCpuParticleSystem = do
  let yolo :: Show a => Either a b -> b
      yolo = either (error.show) id

  logging "newCpuParticleSystem"
  let vtxCapacity = 9
      idxCapacity = 4 * 2 * 3
      instCapacity = cpuParticleMax
      vtxStride = 12
  glErrorToLog "newCpuParticleSystem.enter"
  glVao <- allocateObject (glGenVertexArrays 1)
  glErrorToLog "newCpuParticleSystem.vao"

  count <- liftIO $ newIORef 0

  -- vertex and index buffers
  idx <- storageNew GL_ELEMENT_ARRAY_BUFFER idxCapacity
  vtx <- storageNew GL_ARRAY_BUFFER vtxCapacity
  -- and the instance buffer
  inst <- storageNew GL_SHADER_STORAGE_BUFFER instCapacity

  glErrorToLog "newCpuParticleSystem.storage"
  -- set up the binding for the vertex and instance buffer

  glBindVertexArray glVao
  glBindVertexBuffer 0 (_storeObject vtx) 0 vtxStride
  glBindBuffer GL_ARRAY_BUFFER (_storeObject vtx)
  glEnableVertexAttribArray  0
  glVertexAttribBinding  0 0
  glVertexAttribFormat 0 3 GL_FLOAT GL_FALSE 0
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER (_storeObject idx)
  glBindVertexArray 0

  glErrorToLog "newCpuParticleSystem.vaoSetup"

  -- fill the vertex and index buffers with the particle quads
  liftIO $ VS.unsafeWith cpuParticleVertices $ \ptr ->
    unsafeStore vtx vtxCapacity ptr *> pure ()

  liftIO $ VS.unsafeWith cpuParticleIndices $ \ptr ->
    unsafeStore idx idxCapacity ptr *> pure ()

  prog <- yolo <$> loadRenderProgramFromFiles "cpu_particle.vert" "cpu_particle.frag" "cpu_particle" Nothing

  (bufferP, bufferI) <- liftIO $ do
    bp <- VSM.new instCapacity
    bi <- VSM.new instCapacity
    pure (bp,bi)
  spawnQ <- liftIO $ newTQueueIO
  tx <- yolo <$> fileTexture2D "./data/textures/vfx/pcl_blobs_4x4.png" NonResident
  pure $ CpuParticleSystem
    { _cpuParticleVertices = vtx
    , _cpuParticleIndices = idx
    , _cpuParticleInstances = inst
    , _cpuParticleVao = glVao
    , _cpuParticleProgram = prog
    , _cpuParticleCount = count
    , _cpuParticleBuffer = bufferP
    , _cpuParticleInstanceBuffer = bufferI
    , _cpuParticleSpawning = spawnQ
    , _cpuParticleTexture = tx
    }


-- this is an entirely dumb setup
-- update (and spawning) could be handled by sparking off separate threads
-- rather than constantly resetting and overwriting a linear buffer, a circular
-- buffer should be used and written to with GL_MAP_PERSISTENT_BIT/GL_MAP_COHERENT_BIT
updateCpuParticleSystem :: (MonadIO m) => CpuParticleSystem -> Float -> V3 Float -> m ()
updateCpuParticleSystem pcls dt camPos = liftIO $ do
    count <- readIORef (_cpuParticleCount pcls)
    -- update extant particles
    live <- updateLoop (count-1) (count-1) 0
    -- spawn new particles
    spawns <- atomically $ flushTQueue (_cpuParticleSpawning pcls)
    live' <- foldlM spawn live spawns
    -- sort the live particles in-place (hence unsafeSlice)
    let living = VSM.unsafeSlice 0 live' (_cpuParticleBuffer pcls)
    VA.sortBy distToCam living
    -- copy to instances
    copyLoop 0 live'
    -- copy to GPU buffer
    -- TODO: circle buffer or multiple buffers
    storageClear (_cpuParticleInstances pcls)
    _ <- VSM.unsafeWith (_cpuParticleInstanceBuffer pcls) $ unsafeStore (_cpuParticleInstances pcls) live'
    -- update count
    writeIORef (_cpuParticleCount pcls) live'
  where
    distToCam a b
      | da > db = LT
      | da < db = GT
      | otherwise = EQ
      where
        da = qd (_pclPos a) camPos
        db = qd (_pclPos b) camPos
    spawn :: Int -> PclSpawnParams -> IO Int
    spawn live sp = do
      ps <- debugPclSpawn sp
      let n = VS.length ps
          toSpawn = min n (cpuParticleMax-live)
      flip traverse_ [0 .. toSpawn-1] $ \i -> VSM.unsafeWrite (_cpuParticleBuffer pcls) (live+i) (VS.unsafeIndex ps i)
      pure (live+toSpawn)
    updateLoop :: Int -> Int -> Int -> IO Int
    updateLoop (-1) _ l = pure l
    updateLoop index end l = do
      pcl <- updateParticle dt <$> VSM.unsafeRead (_cpuParticleBuffer pcls) index
      if _pclTtl pcl > 0
        then do
          VSM.unsafeWrite (_cpuParticleBuffer pcls) index pcl
          updateLoop (index-1) end (l+1)
        else do
          when (end > index) $
            VSM.unsafeRead (_cpuParticleBuffer pcls) end >>= VSM.unsafeWrite (_cpuParticleBuffer pcls) index
          updateLoop (index-1) (end-1) l
    copyLoop :: Int -> Int -> IO ()
    copyLoop i n
      | i >= n = pure ()
      | otherwise = do
        pclInstance <$> VSM.unsafeRead (_cpuParticleBuffer pcls) i >>= VSM.unsafeWrite (_cpuParticleInstanceBuffer pcls) i
        copyLoop (i+1) n


{-
  rather than a simple quad, use a subdivided square with a tented middle vertex
  this gives some nice parallax and more interesting intersections with geo, especially
  when soft depth comparisons are used

  +__+__+
  |  |  |
  +--X--+   
  |  |  |
  +--+--+
-}
cpuParticleVertices :: VS.Vector (V3 Float)
cpuParticleVertices = VS.generate 9 go
  where
    go i = V3 (x-1) (y-1) (if i == 4 then 1 else 0)
      where
        x = fromIntegral $ i `mod` 3
        y = fromIntegral $ i `div` 3

cpuParticleIndices :: VS.Vector Word16
cpuParticleIndices = VS.fromList
  [ 0, 4, 3
  , 0, 1, 4
  , 1, 2, 4
  , 2, 5, 4
  , 5, 8, 4
  , 8, 7, 4
  , 7, 6, 4
  , 6, 3, 4
  ]
























data ParticleOrientation
  = FaceCameraPos
  | FaceCameraPlane
  | BillboardX
  | BillboardY
  | BillboardZ
  | BillboardVelocity
  | WorldRotation
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
