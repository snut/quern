module Quern.Render.Query
  ( Query(..), TimeElapsed, TimeStamp
  , generateQuery
  , withElapsedTime
  , queryReady
  , elapsedTimeResult
  ) where

import Control.Monad.IO.Class
import Graphics.GL.Core45
import Graphics.GL.Types

import Foreign

newtype Query a = Query { _query :: GLuint } deriving (Eq, Ord, Show, Read)

-- | Phantom type indicating a query that brakets some GPU work
data TimeElapsed
-- | Phantom type indicating a query that reports the GPU time when it is passed,
-- usually used with multiple other queries to find time differences
data TimeStamp

generateQuery :: MonadIO m => m (Query a)
generateQuery = liftIO $ with (0 :: GLuint) $ \ptr -> glGenQueries 1 ptr *> fmap Query (peek ptr)

-- | Perform some IO actions, hopefully including at least a few bits of GPU work
withElapsedTime :: MonadIO m => Query TimeElapsed -> m a -> m a
withElapsedTime (Query q) body = do
  glBeginQuery GL_TIME_ELAPSED q
  result <- body
  glEndQuery GL_TIME_ELAPSED
  pure result

queryReady :: MonadIO m => Query a -> m Bool
queryReady (Query q) = liftIO $ (== GL_TRUE) <$> (with (GL_FALSE :: GLuint) $ \ptr -> glGetQueryObjectuiv q GL_QUERY_RESULT_AVAILABLE ptr *> peek ptr)

-- | Should be in nanoseconds. Note that this could be blocking if the query has not yet
-- been passed by the GPU.
elapsedTimeResult :: MonadIO m => Query TimeElapsed -> m GLuint64
elapsedTimeResult (Query q) = liftIO $ with (0 :: GLuint64) $ \ptr -> glGetQueryObjectui64v q GL_QUERY_RESULT ptr *> peek ptr
