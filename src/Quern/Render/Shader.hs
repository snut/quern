{-# Language OverloadedStrings #-}

module Quern.Render.Shader
  ( Shader(..)
  , Program(..)
  , useProgram
  , compileShaderSourceFrom
  , loadShaderFromFile
  , linkShaderProgram
  , loadKernelFromFile
  , loadKernelFromFile'WithDefines
  , loadRenderProgramFromFiles
  , loadRenderProgramFromFiles'WithDefines
  , reloadProgram
  ) where

import Control.Monad (when, join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Control.Exception
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower, toUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (traverse_)
import qualified Data.Text as T (pack)
import qualified Data.Vector as V
import SDL as SDL

import Graphics.GL.Core45
import Graphics.GL.Types

import Foreign

import System.FilePath.Posix (takeExtension)
import Quern.Render.Shader.PreProcess
import Quern.Render.GL (allocateObject)

import Quern.Logging

data Shader = Shader
  { _shaderObject :: !GLuint
  , _shaderType :: !GLenum
  , _shaderPath :: !FilePath
  , _shaderDefines :: ![String]
  } deriving (Eq, Ord, Show)
data Program = Program
  { _programObject :: !GLuint
  , _programSourcePaths :: ![FilePath]
  , _programDefines :: ![String]
  } deriving (Eq, Ord, Show)

useProgram :: MonadIO m => Program -> m ()
useProgram = glUseProgram . _programObject

infoLog :: (GLuint -> GLenum -> Ptr GLint -> IO ())
        -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
        -> GLuint
        -> IO ByteString
infoLog glLogLength glLogGet object =
  alloca $ \intptr -> do
    glLogLength object GL_INFO_LOG_LENGTH intptr
    logLength <- peek intptr
    allocaArray (fromIntegral logLength) $ \info -> do
      glLogGet object logLength intptr info
      B.packCStringLen (castPtr info, fromIntegral logLength)

shaderLog, programLog :: GLuint -> IO ByteString
shaderLog  = infoLog glGetShaderiv  glGetShaderInfoLog
programLog = infoLog glGetProgramiv glGetProgramInfoLog

whitespace :: String
whitespace = " \t\n\r\v\NUL"

-- testing: this should depend
shaderPrelude :: [String] -> ByteString
shaderPrelude defines = glslVersion <> defns
  where
    glslVersion = "#version 450\n"
    defns = B.unlines $ B.pack . ("#define " <>) . fmap toUpper <$> filter (not . null) defines


-- | Compile a shader from a bytestring, with explicitly provided `GLenum` specifying
-- the shader stage.
compileShaderSourceFrom :: (HasLogging env, MonadReader env m, MonadIO m) => String -> [String] -> GLenum -> ByteString -> FilePath -> m (Either ByteString Shader)
compileShaderSourceFrom name defines ty source path = do
  shader <- glCreateShader ty
  status <- liftIO $ alloca $
    \statusPtr -> do
      B.useAsCString (shaderPrelude defines) $ \preludePtr ->
        B.useAsCString source $
          \stringPtr -> withArray [preludePtr, stringPtr] $ \stringPtrPtr -> do
            glShaderSource shader 2 stringPtrPtr nullPtr
            glCompileShader shader
            glGetShaderiv shader GL_COMPILE_STATUS statusPtr
      peek statusPtr
  slog <- liftIO $ B.dropWhile (`elem` whitespace) <$> shaderLog shader
  let logBytes = B.length slog
  if status == GL_TRUE
    then do
      when (logBytes > 0) $ do
        loggingString ("compileShaderSourceFrom: warnings from: '" <> name <> "'")
        loggingString (B.unpack slog)
      pure . Right $! Shader shader ty path defines
    else do
      glDeleteShader shader
      loggingString ("compileShaderSourceFrom: errors from '" <> name <> "'")
      loggingString (B.unpack slog)
      loggingString "++++++++++++++"
      loggingString (B.unpack (shaderPrelude defines <> source))
      loggingString "--------------"
      pure $! Left slog


packException :: Exception e => Either e a -> Either ByteString a
packException = either (Left . B.pack . displayException) Right

loadShaderFromFile'WithDefines :: (HasLogging env, MonadReader env m, MonadIO m)=> FilePath -> [String] -> m (Either ByteString Shader)
loadShaderFromFile'WithDefines path defines = case stageFromPath path of
    Nothing -> pure (Left (B.pack "Could not deduce shader stage from path: " <> B.pack path))
    Just stage -> mergeE <$> loadAndCompile stage
  where
    mergeE :: Either IOException (Either ByteString a) -> Either ByteString a
    mergeE = join . packException

    loadAndCompile stage = do
      src <- liftIO $ try (preprocess path) -- (B.readFile path)
      case src of
        Right s -> Right <$> compileShaderSourceFrom path defines stage s path
        Left e -> pure (Left e)

loadShaderFromFile :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> m (Either ByteString Shader)
loadShaderFromFile path = loadShaderFromFile'WithDefines path []


stageFromPath :: FilePath -> Maybe GLenum
stageFromPath path = case toLower <$> takeExtension path of
  ".frag" -> Just GL_FRAGMENT_SHADER
  ".vert" -> Just GL_VERTEX_SHADER
  ".comp" -> Just GL_COMPUTE_SHADER
  ".geom" -> Just GL_GEOMETRY_SHADER
  ".tesc" -> Just GL_TESS_CONTROL_SHADER
  ".tese" -> Just GL_TESS_EVALUATION_SHADER
  _ -> Nothing


-- | Link a collection of shaders into a program.
linkShaderProgram :: (HasLogging env, MonadReader env m, MonadIO m) => [Shader] -> m (Either ByteString Program)
linkShaderProgram shaders = do
  program <- glCreateProgram
  relinkShaderProgram program shaders

detachShaders :: (MonadIO m) => Program -> m ()
detachShaders program = liftIO $ do
  let obj = _programObject program
  numAtt <- allocateObject (glGetProgramiv obj GL_ATTACHED_SHADERS)
  let n = fromIntegral numAtt
  shds <- allocaArray n $ \ptr -> do
    glGetAttachedShaders obj numAtt nullPtr ptr
    traverse (peekElemOff ptr) [0 .. n-1]
  traverse_ (glDetachShader obj) shds


relinkShaderProgram :: (HasLogging env, MonadReader env m, MonadIO m) => GLuint -> [Shader] -> m (Either ByteString Program)
relinkShaderProgram program shaders = do
  mapM_ (glAttachShader program . _shaderObject) shaders
  glLinkProgram program
  (linked, plog) <- liftIO $ alloca $ \intptr -> do
    glGetProgramiv program GL_LINK_STATUS intptr
    linked <- peek intptr
    plog <- B.dropWhile (`elem` whitespace) <$> programLog program
    pure (linked, plog)
  let logBytes = B.length plog
      dfns = nubOrd (shaders >>= _shaderDefines)
  if linked == GL_TRUE
    then do
      when (logBytes > 0) (logging "linkShaderProgram: program link warning" *> loggingString (B.unpack plog))
      pure . Right $ Program program (_shaderPath <$> shaders) dfns
    else do
      logging "linkShaderProgram: program link error!"
      loggingString (B.unpack plog)
      glDeleteProgram program
      pure $ Left plog

-- Given a
--reloadShader s = liftIO (preprocess (_shaderPath s)) >>= compileShaderSourceFrom (_shaderPath s) (_shaderDefines s) (_shaderType s)


reloadProgram :: (HasLogging env, MonadReader env m, MonadIO m) => Program -> m Program
reloadProgram prog = do
  shdsE <- traverse id <$> traverse (flip loadShaderFromFile'WithDefines (_programDefines prog)) (_programSourcePaths prog)
  case shdsE of
    Left _ -> pure prog
    Right shds -> do
      detachShaders prog
      progE <- relinkShaderProgram (_programObject prog) shds
      traverse_ (glDeleteShader . _shaderObject) shds
      case progE of
        Left _ -> pure prog
        Right prog' -> do
          -- loggingString $ "Reloaded shader: " <> show prog
          pure prog'


loadKernelFromFile'WithDefines :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> [String] -> m (Either String Program)
loadKernelFromFile'WithDefines path defines = do
  src <- liftIO $ preprocess path --B.readFile path
  shdE <- compileShaderSourceFrom path defines GL_COMPUTE_SHADER src path
  case shdE of
    Left e -> pure . Left . B.unpack $ e
    Right shd -> do
      progE <- linkShaderProgram [shd]
      glDeleteShader (_shaderObject shd)
      case progE of
        Left e -> pure . Left . B.unpack $ e
        Right p -> do
          loggingString $ "Loaded kernel '" ++ path ++ "'"
          pure $ Right p

loadKernelFromFile :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> m (Either String Program)
loadKernelFromFile path = loadKernelFromFile'WithDefines path []


loadRenderProgramFromFiles :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> FilePath -> String -> Maybe GLuint -> m (Either B.ByteString Program)
loadRenderProgramFromFiles vsFile fsFile progKey maybeOld =
  loadRenderProgramFromFiles'WithDefines vsFile fsFile [] progKey maybeOld

loadRenderProgramFromFiles'WithDefines :: (HasLogging env, MonadReader env m, MonadIO m) => FilePath -> FilePath -> [String] -> String -> Maybe GLuint -> m (Either B.ByteString Program)
loadRenderProgramFromFiles'WithDefines vsFile fsFile defines progKey maybeOld = runExceptT $ do
  startProg <- liftIO $ SDL.time
  let vsPath = "./data/shaders/" ++ vsFile
  vsSrc <- liftIO (preprocess vsPath) --B.readFile vsPath
  vs <- ExceptT $ compileShaderSourceFrom (progKey ++ "_vs") defines GL_VERTEX_SHADER vsSrc vsPath
  glErrorToLog $ T.pack ("loadProgram.vertex: " ++ progKey)

  program <- if null fsFile
    then do
      program <- ExceptT $ linkShaderProgram [vs]
      glErrorToLog $ T.pack ("loadProgram.program: "++ progKey)
      liftIO $ glDeleteShader (_shaderObject vs)
      pure program
    else do
      let fsPath = "./data/shaders/" ++ fsFile
      fsSrc <- liftIO (preprocess fsPath)
      fs <- ExceptT $ compileShaderSourceFrom (progKey ++ "_fs") defines GL_FRAGMENT_SHADER fsSrc fsPath
      glErrorToLog $ T.pack ("loadProgram.fragment: "++ progKey)
      program <- ExceptT $  linkShaderProgram [vs, fs]
      glErrorToLog $ T.pack ("loadProgram.program: "++ progKey)
      liftIO $ mapM_ (glDeleteShader . _shaderObject) [vs, fs]
      pure program

  liftIO $ mapM_ glDeleteProgram maybeOld
  endProg <- liftIO $ SDL.time
  let elapsed = endProg - startProg :: Double
  loggingString $ "Loaded program '" ++ progKey ++ "' in " ++ showMS elapsed
  pure program
