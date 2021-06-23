{-| Started thinking about a vanilla GPU particle system -}

{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}

module Quern.Render.Particles.Types
  ( ParticleBuffer
  , ParticleTypeIndex
  , ParticleType(..)
  , ParticleSpawn(..)
  , SpawnDist(..)
  , newParticleBuffer
  , addParticleType
  , spawnParticles
  , invalidParticleType
  ) where

import Control.Monad.IO.Class
import Graphics.GL.Core45
import Graphics.GL.Types
import Quern.Render.Internal.StorageBuffer
import Linear
import Foreign
import Numeric.Half

import Control.Monad.State

newtype ParticleTypeIndex = PTI Word16 deriving (Eq, Ord, Show, Storable)

invalidParticleType :: ParticleTypeIndex
invalidParticleType = PTI maxBound

data GPUParticle = GPUParticle
  { _gpuParticlePos_Life :: !(V4 Float)
  , _gpuParticleVel_Rot :: !(V4 Float)
  , _gpuParticleType :: !ParticleTypeIndex
  , _gpuParticleSeed :: !Word16
  }
instance Storable GPUParticle where
  sizeOf ~(GPUParticle p v i sd) = sizeOf p + sizeOf v + sizeOf i + sizeOf sd
  alignment _ = alignment (undefined :: V4 Float)
  peek ptr = do
    let v4 = sizeOf (undefined :: V4 Float)
        w16 = sizeOf (undefined :: Word16)
    pos <- peek (castPtr ptr)
    vel <- peek (ptr `plusPtr` v4)
    idx <- peek (ptr `plusPtr` (v4+v4))
    sd  <- peek (ptr `plusPtr` (v4+v4+w16))
    pure $ GPUParticle pos vel idx sd
  poke ptr (GPUParticle pos vel idx sd) = do
    let v4 = sizeOf (undefined :: V4 Float)
        w16 = sizeOf (undefined :: Word16)
    poke (castPtr ptr) pos
    poke (ptr `plusPtr` v4) vel
    poke (ptr `plusPtr` (v4+v4)) idx
    poke (ptr `plusPtr` (v4+v4+w16)) sd

data DistShape
  = DistBall
  | DistBox
  deriving (Eq, Ord, Show, Enum)

data SpawnDist = SpawnDist
  { _spawnDistShape :: !DistShape
  , _spawnDistPad :: !(V3 Half)
  , _spawnDistParams :: !(V2 (V4 Half))
  } deriving (Eq, Ord, Show)
   -- ball: radius inner/outer, theta inner/outer, phi inner/outer
   -- box: xyz extents inner/outer?

instance Storable SpawnDist where
  sizeOf _ = 12 * sizeOf (undefined :: Half)
  alignment _ = 1
  peek ptr = do
    shp <- peek (castPtr ptr :: Ptr Word16)
    pad <- peek (ptr `plusPtr` 2)
    parms <- peek (ptr `plusPtr` 8)
    pure $ SpawnDist (toEnum (fromIntegral shp)) pad parms
  poke ptr (SpawnDist shp pad parms) = do
    poke (castPtr ptr :: Ptr Word16) (fromIntegral (fromEnum shp))
    poke (ptr `plusPtr` 2) pad
    poke (ptr `plusPtr` 8) parms

data ParticleSpawn = ParticleSpawn
  { _particleSpawnTransform :: !(V3 (V4 Float))
  , _particleSpawnPos :: !SpawnDist
  , _particleSpawnVel :: !SpawnDist
  , _particleSpawnFlags :: !Word32 -- hmm
  , _particleSpawnType :: !ParticleTypeIndex
  , _particleSpawnCount :: !Word16
  } deriving (Eq, Ord, Show)

peekPlus :: forall a. (Storable a) => Ptr Word8 -> IO (a, Ptr Word8)
peekPlus ptr = do
  let ptrA = castPtr ptr :: Ptr a
  a <- peek ptrA
  pure (a, castPtr (advancePtr ptrA 1))

pokePlus :: forall a. (Storable a) => a -> Ptr Word8 -> IO ((), Ptr Word8)
pokePlus a ptr = do
  let ptrA = castPtr ptr :: Ptr a
  poke ptrA a
  pure ((), castPtr (advancePtr ptrA 1))


peekS :: (Storable a) => StateT (Ptr Word8) IO a
peekS = StateT peekPlus

pokeS :: Storable a => a -> StateT (Ptr Word8) IO ()
pokeS = StateT . pokePlus

instance Storable ParticleSpawn where
  sizeOf ~(ParticleSpawn t p v l ty ct) = sizeOf t + sizeOf p + sizeOf v + sizeOf l + sizeOf ty + sizeOf ct
  alignment _ = 1
  peek ptr = flip evalStateT (castPtr ptr) $ do
    tf <- peekS
    p  <- peekS
    v  <- peekS
    l  <- peekS
    ty <- peekS
    ct <- peekS
    pure $ ParticleSpawn tf p v l ty ct
  poke ptr (ParticleSpawn tf p v l ty ct) = flip evalStateT (castPtr ptr) $ do
    pokeS tf
    pokeS p
    pokeS v
    pokeS l
    pokeS ty
    pokeS ct


data ParticleType = ParticleType
  { _particleTypeLifespan :: !(V2 Half) -- min / max lifespan
  , _particleTypeCoeffRest :: !Half
  , _particleTypeDrag :: !Half  -- 4 x half, 8b
  , _particleTypeSize :: !(V4 Half) -- 16b
  , _particleTypeColour :: !(V4 (V4 Half)) -- spawn/mid/death colour bezier, 48 +32b
  , _particleTypeColourRange :: !(V2 (V4 Half)) -- randomisation offset begin/end, 64 +16b
  , _particleTypeCollideSwitch :: !ParticleTypeIndex -- 66 +2b
  , _particleTypeFadeSwitch :: !ParticleTypeIndex -- 68 +2b particle type to use on lifespan expire
  } deriving (Eq, Ord, Show)

instance Storable ParticleType where
  sizeOf ~(ParticleType ls cr drg sz clrA clrB cld fd) = sizeOf ls + sizeOf cr + sizeOf drg + sizeOf sz + sizeOf clrA + sizeOf clrB + sizeOf cld + sizeOf fd
  alignment _ = 1
  peek ptr = flip evalStateT (castPtr ptr) $ do
    ls   <- peekS
    cr   <- peekS
    drg  <- peekS
    sz   <- peekS
    clrA <- peekS
    clrB <- peekS
    cld  <- peekS
    fd   <- peekS
    pure $ ParticleType
      { _particleTypeLifespan = ls
      , _particleTypeCoeffRest = cr
      , _particleTypeDrag = drg
      , _particleTypeSize = sz
      , _particleTypeColour = clrA
      , _particleTypeColourRange = clrB
      , _particleTypeCollideSwitch = cld
      , _particleTypeFadeSwitch = fd
      }
  poke ptr pt = flip evalStateT (castPtr ptr) $ do
    pokeS $ _particleTypeLifespan pt
    pokeS $ _particleTypeCoeffRest pt
    pokeS $ _particleTypeDrag pt
    pokeS $ _particleTypeSize pt
    pokeS $ _particleTypeColour pt
    pokeS $ _particleTypeColourRange pt
    pokeS $ _particleTypeCollideSwitch pt
    pokeS $ _particleTypeFadeSwitch pt

data ParticleBuffer = ParticleBuffer
  { _particleBufferObject :: !GLuint
  , _particleBufferAtomics :: !GLuint
  , _particleBufferSpawn :: !(StorageBuffer ParticleSpawn)
  , _particleBufferTypes :: !(StorageBuffer ParticleType)
  }

newParticleBuffer :: MonadIO m => m ParticleBuffer
newParticleBuffer = do
  let count = 1024*1024
      flags = 0
      spawnCount = 256
      typeCount = 1024
  pcls  <- newBufferObject GL_SHADER_STORAGE_BUFFER (count * sizeOf (undefined :: GPUParticle)) flags
  atoms <- newBufferObject GL_ATOMIC_COUNTER_BUFFER (2 * sizeOf (0 :: GLuint)) flags
  spawn <- storageNew GL_SHADER_STORAGE_BUFFER spawnCount
  types <- storageNew GL_SHADER_STORAGE_BUFFER typeCount
  pure $ ParticleBuffer pcls atoms spawn types

addParticleType :: MonadIO m => ParticleBuffer -> ParticleType -> m ParticleTypeIndex
addParticleType buffer ty = do
  ix <- liftIO $ with ty (unsafeStore (_particleBufferTypes buffer) 1)
  pure . PTI . fromIntegral $ ix

spawnParticles :: MonadIO m => ParticleBuffer -> ParticleSpawn -> m ()
spawnParticles _buffer _spawn = pure ()

{-

-- quick ghci tests

dist = SpawnDist DistBall 0 (V2 (V4 1 2 3 4) (V4 5 6 7 8))
spawn = ParticleSpawn (V3 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0)) dist dist 0 (PTI 3) 42
pclType = ParticleType
  { _particleTypeLifespan = 10
  , _particleTypeCoeffRest = 0.75
  , _particleTypePadding = 0
  , _particleTypeSize = 0.5
  , _particleTypeColour = 1
  , _particleTypeColourRange = 1
  , _particleTypeCollideSwitch = invalidParticleType
  , _particleTypeFadeSwitch = invalidParticleType
  }

storableRoundtrip :: (Eq a, Storable a) => a -> IO Bool
storableRoundtrip a = alloca $ \ptr -> poke ptr a *> peek ptr >>= \a' -> pure (a' == a)
-}
