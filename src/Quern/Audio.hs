{-# Language RankNTypes, GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-| Very much WIP -}

module Quern.Audio
  ( openMixer
  , closeMixer
  , playMixed
  , playOnce
  , playLooped
  , startMusic
  , AudioContext
  , HasAudioContext(..)

  , Priority(..)
  , AudioKey
  -- * Re-exports
  , Mixer.Times
  , Mixer.Channel
  ) where

-- WIP!
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Lens (Traversal', lens, preview)
import SDL.Mixer as Mixer
import SDL.Init as SDL
import qualified Data.Map as M
import Data.Map (Map)
--import Quern.Types
-- -------------------------------------------------------
-- TODO: Refactor into a bunch of (MonadReader env m, MonadIO m, HasAudioContext env) type of dealies


mixerSpec :: Audio
mixerSpec = Audio
  { audioFrequency = 22050
  , audioFormat = Mixer.FormatS16_Sys
  , audioOutput = Mixer.Stereo
  }

newtype Priority = Priority Int deriving (Eq, Ord, Show, Read, Num)
type AudioKey = String

data AudioContext = AudioContext
  { _audioMusic :: !(Maybe Music)
  , _audioChunks :: !(Map AudioKey Chunk)
  } deriving (Eq, Show)

class HasAudioContext a where
  audioContext :: Traversal' a AudioContext

instance HasAudioContext AudioContext where
  audioContext = lens id const


openMixer :: MonadIO m => m AudioContext
openMixer = do
  -- should consider gathering all the SDL subsystem inits in one place...
  SDL.initialize [InitAudio]
  Mixer.initialize [InitMP3, InitOGG]
  let chunkSize = 4096 -- too much latency?
  Mixer.openAudio mixerSpec chunkSize
  --music <- Mixer.load musicFile
  Mixer.setMusicVolume 8 -- max volume is 128
  --Mixer.playMusic Forever music
  let chCount = 16
  Mixer.setChannels chCount
  flip mapM_ [0 .. chCount-1] $ \i ->
    Mixer.setVolume 16 (fromIntegral i :: Channel)
  -- decide how to handle lifetime of sounds, might be few enough they can all be loaded...
  --snd <- Mixer.load sndFile
  pure $ AudioContext
    { _audioMusic = Nothing --Just music
    , _audioChunks = M.fromList [] --("snd", snd)
    }

playMixed :: MonadIO m => Times -> Channel -> String -> AudioContext -> m (Maybe Int)
playMixed times ch eff ctx = case M.lookup eff (_audioChunks ctx) of
  Just fx -> Just . fromIntegral <$> Mixer.playOn ch times fx
  Nothing -> pure Nothing

playOnce', playLooped' :: MonadIO m => Channel -> String -> AudioContext -> m (Maybe Int)
playOnce' = playMixed 1
playLooped' = playMixed 0


playOnce :: (MonadReader env m, MonadIO m, HasAudioContext env) => AudioKey -> Priority -> m ()
playOnce k _ = do
  mctx <- preview audioContext
  case mctx of
    Just ctx -> do
      mch <- getAvailable DefaultGroup
      case mch of
        Just ch -> playOnce' ch k ctx *> pure ()
        Nothing -> do
          mch' <- getOldest DefaultGroup
          case mch' of
            Just ch' -> playOnce' ch' k ctx *> pure ()
            Nothing -> pure ()
    Nothing -> pure ()

playLooped :: (MonadReader env m, MonadIO m, HasAudioContext env) => AudioKey -> Priority -> m Channel
playLooped k _ = do
  ctx <- preview audioContext
  case ctx of
    Just c -> playLooped' 1 k c *> pure ()
    Nothing -> pure ()
  pure 1

startMusic :: (MonadReader env m, MonadIO m, HasAudioContext env) => FilePath -> m ()
startMusic musicFile = do
  ctx <- preview audioContext
  case ctx of
    Just _ -> do
      pl <- Mixer.playingMusic
      when (not pl) $ do
        music <- Mixer.load musicFile
        Mixer.playMusic Forever music
    Nothing -> pure ()
    -- ??? using music assigned to the context during creation
    -- throws up a gnarly error about invalid RVA mode
    -- maybe the music gets free'd?
    --mapM_ (Mixer.playMusic Forever) (ctx >>= _audioMusic)


closeMixer :: MonadIO m => AudioContext -> m ()
closeMixer ctx = do
  Mixer.haltMusic
  Mixer.closeAudio
  mapM_ Mixer.free (_audioMusic ctx)
  mapM_ Mixer.free (_audioChunks ctx)
  Mixer.quit





--
