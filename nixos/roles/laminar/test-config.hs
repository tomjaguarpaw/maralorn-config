{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-missing-signatures -Wno-type-defaults -Wno-orphans #-}

import Control.Concurrent.Async
import Data.String.Interpolate
import Data.Text (stripPrefix)
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
import Relude
import Say
import Shh
import System.Environment

load Absolute ["laminarc", "git", "nix-instantiate"]

repo = "git@hera.m-0.eu:nixos-config"

jobs :: [String]
jobs =
  $$( liftTyped
        =<< runIO
          ( do
              homes <- getEnv "HOMES"
              systems <- getEnv "SYSTEMS"
              let ret =
                    ((\x -> [i|system-config-#{x}|]) <$> (words . toText) systems)
                      <> ((\x -> [i|home-config-#{x}|]) <$> (words . toText) homes)
              say [i|Found jobs #{ret}|]
              pure ret
          )
    )

deployCommand :: String
deployCommand = $$(liftTyped =<< runIO (getEnv "DEPLOY"))

main = do
  let process = fromMaybe "master" . (stripPrefix "refs/heads/" . toText =<<)
  branch <- process <$> lookupEnv "BRANCH"
  jobId <- getEnv "JOB"
  runId <- getEnv "RUN"
  git "clone" repo "."
  git "checkout" (toString branch)
  (Text.dropAround ('"' ==) . decodeUtf8 . trim -> derivationName) <- nix_instantiate "test.nix" |> captureTrim
  setEnv "LAMINAR_REASON" [i|Building config branch #{branch} for all systems in #{jobId}:#{runId}|]
  say [i|Starting builds of branch #{branch} for all systems.|]
  concurrently_ (mapConcurrently_ (\x -> laminarc ["run", x, [i|BRANCH=#{branch}|]]) jobs) $ exe "nix-jobs" ["realise", toString derivationName]
  say [i|Builds succeeded.|]
  when (branch == "master") $ do
    say [i|Deploying new config to localhost.|]
    exe "/run/wrappers/bin/sudo" deployCommand
  when (branch == "niv-bump") $ do
    say [i|Merging branch niv-bump into master.|]
    git "checkout" "master"
    git "merge" "origin/niv-bump"
    git "push"
