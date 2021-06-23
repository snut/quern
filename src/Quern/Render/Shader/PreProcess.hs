{-# Language OverloadedStrings #-}
module Quern.Render.Shader.PreProcess
  ( preprocess
  ) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char (isSpace)
import System.FilePath.Posix

-- | Do some stupid pre-processing of shader source files
preprocess :: FilePath -> IO ByteString
preprocess pth = B.readFile pth >>= includes wd
  where
    wd = takeDirectory pth

includes :: FilePath -> ByteString -> IO ByteString
includes workingDir src = includeStep workingDir src

-- technically grabs the first lexeme, meh
trim :: ByteString -> ByteString
trim = B.takeWhile (not . isSpace) . B.dropWhile isSpace

-- includes are usually wrapped in "file.h" or <file.h>
unquote :: ByteString -> ByteString
unquote strWs
  | isQuote (B.head str) && isQuote (B.last str) = B.take (n-2) . B.drop 1 $ str
  | otherwise = str
  where
    n = B.length str
    isQuote = (`elem` ("\"<>" :: String))
    str = trim strWs

-- look for include-like things
include :: ByteString -> Maybe FilePath
include str
  | prefix `B.isPrefixOf` str = Just . B.unpack . unquote . B.drop (B.length prefix) $ str
  | otherwise = Nothing
  where
    prefix = "#include"

-- one level of include splicing
includeStep :: FilePath -> ByteString -> IO ByteString
includeStep workingDir src = do
    loc' <- traverse expand loc
    pure $ B.unlines loc'
  where
    loc = B.lines src
    expand l = case include l of
      Just fp -> B.readFile (workingDir </> fp)
      Nothing -> pure l
