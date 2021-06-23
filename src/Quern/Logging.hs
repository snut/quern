{-# Language ConstraintKinds #-}

module Quern.Logging
  ( logging
  , loggingString
  , startNewLogFile
  , stopLogging
  , LoggingHandle
  , HasLogging(..)
  , glErrorToLog
  , showMS
  , unliftLoggingString
  , logHistory
  -- * Equivalents when the logging handle is part of a stateful context
  , logging'
  , loggingString'
  --
  , MonadReader
  , MonadIO
  ) where


-- grab the app monad
import Quern.Types
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T (pack)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import System.IO
import Control.Lens
import Graphics.GL.Core45
import Numeric (showHex, showFFloat)

unliftLoggingString :: (MonadIO m, MonadIO n, HasLogging env, MonadReader env m)
          => m (String -> n ())
unliftLoggingString = do
  hdl <- view loggingHandle
  pure ((`logLn` hdl) . T.pack)

succWrap :: Int -> Int -> (Int, Int)
succWrap n i = (j, i)
  where j = (succ i) `mod` n

logLn :: MonadIO m => Text -> LoggingHandle -> m ()
logLn msg (LoggingHandle hdl hist pos) = liftIO $ do
  T.hPutStrLn hdl msg
  hFlush hdl
  i <- atomicModifyIORef' pos (succWrap (VM.length hist))
  VM.unsafeWrite hist i msg

logLnStr :: MonadIO m => String -> LoggingHandle -> m ()
logLnStr s = logLn (T.pack s)
--logLnStr msg (LoggingHandle hdl) = liftIO $ hPutStrLn hdl msg *> hFlush hdl

startNewLogFile :: MonadIO m => FilePath -> m LoggingHandle
startNewLogFile path = liftIO $ do
  hdl <- openFile path WriteMode
  let n = 16
  vm <- VM.replicate n mempty
  p <- newIORef 0
  pure $ LoggingHandle hdl vm p

stopLogging :: MonadIO m => LoggingHandle -> m ()
stopLogging = liftIO . hClose . _loggingHandle

-- | Environments with a log handle
class HasLogging a where
  loggingHandle :: Lens' a LoggingHandle

data SP a b = SP !a !b deriving (Eq, Show, Read, Ord)

-- things to consider:
-- log once (keep a set of text)
-- log verbosity

data LoggingHandle = LoggingHandle
  { _loggingHandle :: !Handle
  , _loggingHistory :: !(VM.IOVector Text)
  , _loggingHistoryPos :: !(IORef Int)
  }

instance HasLogging LoggingHandle where loggingHandle = id

-- not necessarily consistent across threads
logHistory :: (HasLogging env, MonadReader env m, MonadIO m) => m (V.Vector Text)
logHistory = do
  hdl <- view loggingHandle
  liftIO $ do
    i <- readIORef (_loggingHistoryPos hdl)
    let n = VM.length (_loggingHistory hdl)
    V.generateM n $ \j -> let k = (j+i) `mod` n in VM.unsafeRead (_loggingHistory hdl) k

--type MonadLogging m = (HasLogging env, MonadReader env m, MonadIO m)

-- | Log a message to a file handle stored in the environment
--logging :: HasLogging env => Text -> Quern env ()
logging :: (HasLogging env, MonadReader env m, MonadIO m) => Text -> m ()
logging msg = view loggingHandle >>= logLn msg

loggingString :: (HasLogging env, MonadReader env m, MonadIO m) => String -> m ()
loggingString msg = view loggingHandle >>= logLnStr msg

logging' :: (HasLogging st, MonadState st m, MonadIO m) => Text -> m ()
logging' msg = use loggingHandle >>= logLn msg

loggingString' :: (HasLogging st, MonadState st m, MonadIO m) => String -> m ()
loggingString' msg = use loggingHandle >>= logLnStr msg

errorPrefix :: Text
errorPrefix = T.pack "GL_ERROR|> "


glErrorToLog :: (HasLogging env, MonadReader env m, MonadIO m) => Text -> m ()
glErrorToLog context = let logE = logging . (errorPrefix <>) . (context <>) . T.pack . (' ':) in do
  e <- glGetError
  case e of
    GL_NO_ERROR -> pure ()
    GL_INVALID_ENUM -> logE "GL_INVALID_ENUM"
    GL_INVALID_VALUE -> logE "GL_INVALID_VALUE"
    GL_INVALID_OPERATION -> logE "GL_INVALID_OPERATION"
    GL_INVALID_FRAMEBUFFER_OPERATION -> logE "GL_INVALID_FRAMEBUFFER_OPERATION"
    GL_OUT_OF_MEMORY -> logE "GL_OUT_OF_MEMORY"
    GL_STACK_UNDERFLOW -> logE "GL_STACK_UNDERFLOW"
    GL_STACK_OVERFLOW -> logE "GL_STACK_OVERFLOW"
    _ -> logE $! "Unknown enum value: 0x" ++ showHex e ""


-- converts a time in seconds to milliseconds and displays it
showMS :: RealFloat a => a -> String
showMS x = showFFloat (Just 2) (x * 1000) " ms"
