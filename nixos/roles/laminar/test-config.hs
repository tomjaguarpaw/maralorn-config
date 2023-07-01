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

-- import Language.Haskell.TH.Syntax
import Relude
import Say
import Shh

-- import System.Environment (getEnv)

load Absolute ["git", "nix", "builders-configurator", "archive-nix-path"]

repo = "git@hera.m-0.eu:nixos-config"

-- deployCommand :: String
-- deployCommand = $$(bindCode (runIO $ getEnv "DEPLOY") liftTyped)

main = do
  [] <- missingExecutables
  let process = fromMaybe "main" . (stripPrefix "refs/heads/" . toText =<<)
  branch <- process <$> lookupEnv "BRANCH"
  git "clone" repo "."
  git "checkout" (toString branch)
  git "show" "-q"
  say "Running checks"
  builders <- builders_configurator |> captureTrim
  nix "flake" "check" "--builders" ([i|@#{builders}|] :: String) "--accept-flake-config" "-L"
  nix ["build", ".#checks.x86_64-linux.system-checks", "--builders", [i|@#{builders}|], "--accept-flake-config", "-L"]
  archive_nix_path "./result"
  say "Checks succeeded"
  -- when (branch == "main") $ do
  --  say [i|Deploying new config to localhost.|]
  --  exe "/run/wrappers/bin/sudo" deployCommand
  when (branch == "flake-lock-update") $ do
    say [i|Merging branch flake-lock-update into main.|]
    git "checkout" "main"
    git "merge" "origin/flake-lock-update"
    git "push"
