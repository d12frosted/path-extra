{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Path.Parse
       (module Path
       ,parseAbsDir
       ,parseRelDir
       ,parseDirPath
       ,parseAbsFile
       ,parseRelFile
       ,parseFilePath
       ,getWorkingDir) where

--------------------------------------------------------------------------------
-- * Internal imports

--------------------------------------------------------------------------------
-- * External imports

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable
import           Env
import           Path                   hiding (PathParseException, parseAbsDir,
                                         parseAbsFile, parseRelDir,
                                         parseRelFile)
import qualified Path                   as P (parseAbsDir, parseAbsFile,
                                              parseRelDir, parseRelFile)
import           Prelude
import qualified System.Directory       as D

--------------------------------------------------------------------------------
-- * PathIOException definition

data PathIOException
  = InvalidDir FilePath
  | InvalidFile FilePath
  deriving (Typeable)

instance Exception PathIOException

instance Show PathIOException where
  show (InvalidDir fp)  = "'" ++ fp ++ "' is not a directory."
  show (InvalidFile fp) = "'" ++ fp ++ "' is not a file."

--------------------------------------------------------------------------------
-- * Parsers

-- | Get a location for an absolute directory. Produces a normalized
--  path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseAbsDir :: (MonadThrow m) => Text -> m (Path Abs Dir)
parseAbsDir = P.parseAbsDir . Text.unpack

-- | Get a location for a relative directory. Produces a normalized
-- path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseRelDir :: (MonadThrow m) => Text -> m (Path Rel Dir)
parseRelDir = P.parseRelDir . Text.unpack

-- | Parse a directory path. If it's relative, then the absolute version
-- is yielded, based off the working directory. Supports environment variables
-- and '~/' in input.
--
-- Throws: 'PathParseException', 'PathIOException'
--
parseDirPath :: (MonadThrow m, MonadIO m) => Text -> m (Path Abs Dir)
parseDirPath fp = P.parseAbsDir =<< verifyDir =<< canonicalizePath =<< toAbsFilePath fp

-- | Get a location for an absolute file.
--
-- Throws: 'PathParseException'
--
parseAbsFile :: (MonadThrow m) => Text -> m (Path Abs File)
parseAbsFile = P.parseAbsFile . Text.unpack

-- | Get a location for a relative file.
--
-- Throws: 'PathParseException'
--
parseRelFile :: (MonadThrow m) => Text -> m (Path Rel File)
parseRelFile = P.parseRelFile . Text.unpack

-- | Parse a file path. If it's relative, then the absolute version
-- is yielded, based off the working directory. Supports environment variables
-- and '~/' in input.
--
-- Throws: 'PathParseException', 'PathIOException'
--
parseFilePath :: (MonadThrow m, MonadIO m) => Text -> m (Path Abs File)
parseFilePath fp = P.parseAbsFile =<< verifyFile =<< canonicalizePath =<< toAbsFilePath fp

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
    do wd <- liftIO $ D.canonicalizePath "."
       return $ Text.concat [Text.pack wd,"/",fp]

-- | Canonicalize path with support of environment variables and '~/'.
canonicalizePath :: (MonadThrow m, MonadIO m) => Text -> m FilePath
canonicalizePath fp = expandPath fp >>=
                      liftIO . D.canonicalizePath . Text.unpack

-- | Expand path by expanding all extracted atoms. In other words, replace all
-- environment variables by their valus and `~/` by value of $HOME.
expandPath :: (MonadThrow m, MonadIO m) => Text -> m Text
expandPath fp = liftM (Text.intercalate "/") (mapM expandAtom (extractAtoms fp))

-- | Extract atoms of given path.
extractAtoms :: Text -> [Text]
extractAtoms = Text.splitOn "/"

-- | Expand atom.
expandAtom :: (MonadThrow m, MonadIO m) => Text -> m Text
expandAtom atom
  | Text.isPrefixOf "$" atom = env $ strip "$" atom
  | Text.isPrefixOf "~" atom = env "HOME"
  | otherwise = pure atom
  where strip p v =
          fromMaybe v $ Text.stripPrefix p v

-- | Verify FilePath for being directory.
--
-- Throws: 'PathIOException'
--
verifyDir :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
verifyDir fp =
  liftIO (D.doesDirectoryExist fp) >>=
  \case
    True -> return fp
    False -> throwM $ InvalidDir fp

-- | Verify FilePath for being file.
--
-- Throws: 'PathIOException'
--
verifyFile :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
verifyFile fp =
  liftIO (D.doesFileExist fp) >>=
  \case
    True -> return fp
    False -> throwM $ InvalidFile fp
