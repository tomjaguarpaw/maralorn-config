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
{-# OPTIONS_GHC -Wall -Werror -Wno-missing-signatures -Wno-type-defaults #-}

import Data.String.Interpolate
import Data.Text (stripPrefix)
import Language.Haskell.TH.Syntax
import Relude
import Say
import Shh
import System.Environment (getEnv)

load Absolute ["laminarc", "git", "nix"]

repo = "git@hera.m-0.eu:nixos-config"

deployCommand :: String
deployCommand = $$(bindCode (runIO $ getEnv "DEPLOY") liftTyped)

main = do
  let process = fromMaybe "main" . (stripPrefix "refs/heads/" . toText =<<)
  branch <- process <$> lookupEnv "BRANCH"
  git "clone" repo "."
  git "checkout" (toString branch)
  say "Running checks"
  nix "flake" "check" "--builders" "@/etc/nix/machines"
  nix ["build", ".#checks.x86_64-linux.system-checks", "-o", "/var/cache/gc-links/test-config", "--builders", "@/etc/nix/machines"]
  say "Checks succeeded"
  when (branch == "main") $ do
    say [i|Deploying new config to localhost.|]
    exe "/run/wrappers/bin/sudo" deployCommand
  when (branch == "niv-bump") $ do
    say [i|Merging branch niv-bump into main.|]
    git "checkout" "main"
    git "merge" "origin/niv-bump"
    git "push"
