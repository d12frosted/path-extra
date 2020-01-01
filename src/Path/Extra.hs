-- | Extra stuff for Path module.
--
-- Original Path is reexported as well as Path.Parse module.

--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Path.Extra where

--------------------------------------------------------------------------------

import           Path.Parse

--------------------------------------------------------------------------------

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Prelude

--------------------------------------------------------------------------------

xdgConfigHome :: (MonadIO m, MonadThrow m, MonadCatch m) => m (Path Abs Dir)
xdgConfigHome
  = parseDirPath "$XDG_CONFIG_HOME" `catchAll`
  (const $ parseDirPath "$HOME/.config")

xdgDataHome :: (MonadIO m, MonadThrow m, MonadCatch m) => m (Path Abs Dir)
xdgDataHome
  = parseDirPath "$XDG_DATA_HOME" `catchAll`
  (const $ parseDirPath "$HOME/.local/share")

--------------------------------------------------------------------------------
