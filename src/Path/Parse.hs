--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

-- | This module defines functions to parse type safe file paths from strings
-- containing Unix environment variables (e.g. $HOME).
--
-- In most cases one should use 'parseDirPath' and 'parseFilePath' as they take
-- care of any variables expansion and work also with relative paths successfully
-- converting them into absolute.
module Path.Parse
  ( module Path,
    canonicalizePath,
    getWorkingDir,
    parseAbsDir,
    parseAbsFile,
    parseDirPath,
    parseFilePath,
    parseRelDir,
    parseRelFile,
  )
where

--------------------------------------------------------------------------------

import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Path hiding
  ( parseAbsDir,
    parseAbsFile,
    parseRelDir,
    parseRelFile,
  )
import qualified Path as P
  ( parseAbsDir,
    parseAbsFile,
    parseRelDir,
    parseRelFile,
  )
import qualified System.Directory as D
import System.Environment.Extra
import Prelude

--------------------------------------------------------------------------------
-- Parsers

-- | Parse a directory path. If it's relative, then the absolute version
-- is yielded, based off the working directory. Supports environment variables
-- and '~/' in input.
--
-- Throws: 'PathException'
parseDirPath :: (MonadThrow m, MonadIO m) => Text -> m (Path Abs Dir)
parseDirPath fp = P.parseAbsDir =<< canonicalizePath =<< toAbsFilePath fp

-- | Like 'parseDirPath', but throws an exception when directory doesn't exist.
--
-- Throws: 'PathException', 'PathIOException'
parseDirPath' :: (MonadThrow m, MonadIO m) => Text -> m (Path Abs Dir)
parseDirPath' fp = verifyDir =<< parseDirPath fp

-- | Get a location for an absolute directory. Produces a normalized
--  path which always ends in a path separator.
--
-- Throws: 'PathException'
parseAbsDir :: (MonadThrow m) => Text -> m (Path Abs Dir)
parseAbsDir = P.parseAbsDir . Text.unpack

-- | Get a location for a relative directory. Produces a normalized
-- path which always ends in a path separator.
--
-- Throws: 'PathException'
parseRelDir :: (MonadThrow m) => Text -> m (Path Rel Dir)
parseRelDir = P.parseRelDir . Text.unpack

--------------------------------------------------------------------------------

-- | Parse a file path. If it's relative, then the absolute version
-- is yielded, based off the working directory. Supports environment variables
-- and '~/' in input.
--
-- Throws: 'PathException'
parseFilePath :: (MonadThrow m, MonadIO m) => Text -> m (Path Abs File)
parseFilePath fp = P.parseAbsFile =<< canonicalizePath =<< toAbsFilePath fp

-- | Like 'parseFilePath', but throws an exception when file doesn't exist.
--
-- Throws: 'PathException', 'PathIOException'
parseFilePath' :: (MonadThrow m, MonadIO m) => Text -> m (Path Abs File)
parseFilePath' fp = verifyFile =<< parseFilePath fp

-- | Get a location for an absolute file.
--
-- Throws: 'PathException'
parseAbsFile :: (MonadThrow m) => Text -> m (Path Abs File)
parseAbsFile = P.parseAbsFile . Text.unpack

-- | Get a location for a relative file.
--
-- Throws: 'PathException'
parseRelFile :: (MonadThrow m) => Text -> m (Path Rel File)
parseRelFile = P.parseRelFile . Text.unpack

--------------------------------------------------------------------------------

-- | Get the current working directory.
getWorkingDir :: (MonadIO m) => m (Path Abs Dir)
getWorkingDir = liftIO (D.canonicalizePath "." >>= P.parseAbsDir)

-- | Convert given path to absolute file path.
toAbsFilePath :: (MonadIO m) => Text -> m Text
toAbsFilePath fp
  | Text.isPrefixOf "$" fp = return fp
  | Text.isPrefixOf "~" fp = return fp
  | Text.isPrefixOf "/" fp = return fp
  | otherwise =
    do
      wd <- liftIO $ D.canonicalizePath "."
      return $ Text.concat [Text.pack wd, "/", fp]

-- | Canonicalize path with support of environment variables and '~/'.
canonicalizePath :: (MonadThrow m, MonadIO m) => Text -> m FilePath
canonicalizePath fp =
  expandPath fp
    >>= liftIO . D.canonicalizePath . Text.unpack

-- | Expand path by expanding all extracted atoms. In other words, replace all
-- environment variables by their valus and `~/` by value of $HOME.
expandPath :: (MonadThrow m, MonadIO m) => Text -> m Text
expandPath fp = fmap (Text.intercalate "/") (traverse expandAtom (extractAtoms fp))

-- | Extract atoms of given path.
extractAtoms :: Text -> [Text]
extractAtoms = Text.splitOn "/"

-- | Expand atom.
expandAtom :: (MonadThrow m, MonadIO m) => Text -> m Text
expandAtom atom
  | Text.isPrefixOf "$" atom = getEnv $ strip "$" atom
  | Text.isPrefixOf "~" atom = getEnv "HOME"
  | otherwise = pure atom
  where
    strip p v =
      fromMaybe v $ Text.stripPrefix p v

-- | Verify FilePath for being directory.
--
-- Throws: 'PathIOException'
verifyDir :: (MonadThrow m, MonadIO m) => Path Abs Dir -> m (Path Abs Dir)
verifyDir fp =
  liftIO (D.doesDirectoryExist raw)
    >>= \case
      True -> return fp
      False -> throwM $ InvalidAbsDir raw
  where
    raw = toFilePath fp

-- | Verify FilePath for being file.
--
-- Throws: 'PathIOException'
verifyFile :: (MonadThrow m, MonadIO m) => Path Abs File -> m (Path Abs File)
verifyFile fp =
  liftIO (D.doesFileExist raw)
    >>= \case
      True -> return fp
      False -> throwM $ InvalidAbsFile raw
  where
    raw = toFilePath fp

--------------------------------------------------------------------------------
