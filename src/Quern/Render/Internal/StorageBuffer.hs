{-# Language ScopedTypeVariables #-}

module Quern.Render.Internal.StorageBuffer
  ( -- * Types
    StorageBuffer(..)
  , StorageBufferReport(..)
    -- * Operations on storage buffers
  , storageNew
  , unsafeStore
  , storageFreeSpace
  , storageUsedSpace
  , storageReport
  , storageClear
  -- * Utility to allocate a raw OpenGL buffer object
  , newBufferObject
  , newBufferObjectFromPtr
  -- * Debug
  , debugStorageTake
  , debugBufferTake
  -- * Re-exports
  , GLuint
  , GLenum
  , GLbitfield
  , allocateObject
  ) where


import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Vector.Storable as VS
import Foreign
import Graphics.GL.Core45
import Graphics.GL.Types

import Quern.Render.GL


-- | A 'StorageBuffer' represents a mutable buffer object resident in GPU memory.
-- It is written to in an append-only fashion.
--
-- TODO: Consider moving the _storeReadable to a phantom type parameter here,
-- which most things can be polymorphic on. Gives some safety in trying to read
data StorageBuffer a = StorageBuffer
  { _storeCapacity :: !Int
  , _storeUsage :: !(IORef Int)
  , _storeObject :: !GLuint
  , _storeElemSize :: !Int
  , _storeTarget :: !GLenum
  , _storeReadable :: !Bool
  , _storeSync :: !(IORef GLsync)
  }

data StorageBufferReport = StorageBufferReport
  { _storageBufferReportCapacity :: !Int
  , _storageBufferReportUsage :: !Int
  , _storageBufferReportUsageRatio :: !Float
  } deriving (Eq, Ord, Show)

storageReport :: MonadIO m => StorageBuffer a -> m StorageBufferReport
storageReport store = do
  used <- liftIO $ readIORef (_storeUsage store)
  let ratio = fromIntegral used / fromIntegral (_storeCapacity store)
  pure $! StorageBufferReport (_storeCapacity store) used ratio

newBufferObject :: MonadIO m => GLenum -> Int -> GLbitfield -> m GLuint
newBufferObject target size flags = liftIO $ do
  object <- alloca $ \ptr -> glGenBuffers 1 ptr *> peek ptr
  glBindBuffer target object
  glBufferStorage target (fromIntegral size) nullPtr flags
  glBindBuffer target 0
  pure object

-- | Note that this expects the pointer to reference at least enough data to fill
-- the newly-allocated buffer
newBufferObjectFromPtr :: MonadIO m => GLenum -> Int -> GLbitfield -> Ptr () -> m GLuint
newBufferObjectFromPtr target size flags dataPtr = liftIO $ do
  object <- alloca $ \bptr -> glGenBuffers 1 bptr *> peek bptr
  glBindBuffer target object
  glBufferStorage target (fromIntegral size) dataPtr flags
  glBindBuffer target 0
  pure object

storageNewDebug :: forall a m. (Storable a, MonadIO m) => GLenum -> Int -> m (StorageBuffer a)
storageNewDebug = storageNew'Impl True

storageNew :: forall a m. (Storable a, MonadIO m) => GLenum -> Int -> m (StorageBuffer a)
storageNew = storageNew'Impl False

storageNew'Impl :: forall a m. (Storable a, MonadIO m) => Bool -> GLenum -> Int -> m (StorageBuffer a)
storageNew'Impl dbg target capacity = do
  usage <- liftIO $ newIORef 0
  let flags = GL_DYNAMIC_STORAGE_BIT .|. GL_MAP_WRITE_BIT
      flagsDebug = flags .|. GL_MAP_READ_BIT
      elemSize = sizeOf (undefined :: a)

  object <- newBufferObject target (capacity * elemSize) (if dbg then flagsDebug else flags)

  sync <- liftIO $ fence >>= newIORef
  pure $! StorageBuffer
    { _storeCapacity = capacity
    , _storeUsage = usage
    , _storeObject = object
    , _storeElemSize = elemSize
    , _storeTarget = target
    , _storeSync = sync
    , _storeReadable = dbg
    }

debugStorageTake :: forall a m. (Storable a, MonadIO m) => StorageBuffer a -> Int -> m (VS.Vector a)
debugStorageTake store count
  | not (_storeReadable store) = pure mempty
  | otherwise = do
    glBindBuffer (_storeTarget store) (_storeObject store)
    ptr <- glMapBufferRange (_storeTarget store) 0 (fromIntegral $ count * _storeElemSize store) GL_MAP_READ_BIT
    vec <- liftIO $ VS.generateM count $ peekElemOff (castPtr ptr :: Ptr a)
    _ <- glUnmapBuffer (_storeTarget store)
    pure vec

debugBufferTake :: forall a m. (Storable a, MonadIO m) => GLenum -> GLuint -> Int -> m (VS.Vector a)
debugBufferTake target object count = do
  glBindBuffer target object
  ptr <- glMapBufferRange target 0 (fromIntegral $ count * sizeOf (undefined :: a)) GL_MAP_READ_BIT
  vec <- liftIO $ VS.generateM count $ peekElemOff (castPtr ptr)
  _ <- glUnmapBuffer target
  glBindBuffer target 0
  pure vec


-- | Unsafe! No check is made here to confirm the buffer has enough space
unsafeStore :: MonadIO m => StorageBuffer a -> Int -> Ptr a -> m Int
unsafeStore store elemCount ptr = do
    sync <- liftIO $ readIORef (_storeSync store)
    liftIO $ waitSync sync
    glDeleteSync sync

    elemOffset <- liftIO $ readIORef (_storeUsage store)
    let byteOffset = fromIntegral (elemOffset * _storeElemSize store)
    glNamedBufferSubData (_storeObject store) byteOffset byteCount (castPtr ptr)
    sync' <- liftIO $ fence

    liftIO $ do
      modifyIORef' (_storeUsage store) (+ elemCount)
      writeIORef (_storeSync store) sync'
    pure $ elemOffset
  where
    byteCount = fromIntegral (elemCount * _storeElemSize store)

storageFreeSpace :: MonadIO m => StorageBuffer a -> m Int
storageFreeSpace storage = (_storeCapacity storage -) <$> storageUsedSpace storage

storageUsedSpace :: MonadIO m => StorageBuffer a -> m Int
storageUsedSpace storage = liftIO $ readIORef (_storeUsage storage)

-- Moves the write position to the beginning
storageClear :: MonadIO m => StorageBuffer a -> m ()
storageClear store = liftIO $ writeIORef (_storeUsage store) 0
