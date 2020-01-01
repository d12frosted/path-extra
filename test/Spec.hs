-- | Test specs for 'path-extra'.

--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import           Path.Extra
import           Path.Parse

--------------------------------------------------------------------------------

import           Data.String
import           Prelude
import           System.Environment.Extra
import           Test.Tasty
import           Test.Tasty.HUnit

--------------------------------------------------------------------------------

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain
    $ testGroup "path extra"
    [ testCase "return ~/.config when XDG_CONFIG_HOME is not set" $ do
        home <- getEnv "HOME"
        cfg  <- setEnv "XDG_CONFIG_HOME" "" >> xdgConfigHome
        toFilePath cfg @?= (home <> "/.config/")

    , testCase "return custom XDG_CONFIG_HOME" $ do
        cfg  <- setEnv "XDG_CONFIG_HOME" "/" >> xdgConfigHome
        toFilePath cfg @?= "/"

    , testCase "return ~/.local/share when XDG_DATA_HOME is not set" $ do
        home <- getEnv "HOME"
        cfg  <- setEnv "XDG_CONFIG_HOME" "" >> xdgDataHome
        toFilePath cfg @?= (home <> "/.local/share/")

    , testCase "return custom XDG_DATA_HOME" $ do
        cfg  <- setEnv "XDG_DATA_HOME" "/" >> xdgDataHome
        toFilePath cfg @?= "/"
    ]

--------------------------------------------------------------------------------
