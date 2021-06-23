module Quern.Render.GL where

import Graphics.GL.Core45
import Graphics.GL.Types
import Control.Monad.IO.Class
import Foreign

fence ::  MonadIO m => m GLsync
fence = glFenceSync GL_SYNC_GPU_COMMANDS_COMPLETE 0

waitSync :: MonadIO m => GLsync -> m ()
waitSync sync = do
  isSync <- glIsSync sync
  case isSync of
    GL_TRUE -> do
      x <- glClientWaitSync sync GL_SYNC_FLUSH_COMMANDS_BIT (10^(6::Int))
      case x of
        GL_ALREADY_SIGNALED -> pure ()
        GL_CONDITION_SATISFIED -> pure ()
        _ -> waitSync sync
    _ -> pure ()

allocateObject :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
allocateObject body = liftIO $ alloca $ \ptr -> body ptr *> peek ptr
