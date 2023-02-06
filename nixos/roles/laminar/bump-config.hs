{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-missing-signatures -Wno-type-defaults -Wno-orphans #-}

import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate
import Data.Text qualified as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude
import Say
import Shh
import System.Environment

load Absolute ["git", "nix"]
paths :: [Text]
paths =
  $$( bindCode (runIO pathBinsAbs) \rawPaths ->
        let wantedPaths :: [Text] = mapMaybe (\x -> foldr (<|>) Nothing $ (\bin -> Text.stripSuffix [i|/#{bin}|] $ toText x) <$> ["git", "tar", "nix-prefetch-url", "gzip", "ssh"]) rawPaths
         in liftTyped wantedPaths
    )

repo = "git@hera.m-0.eu:nixos-config"

main = do
  git "clone" repo "."
  git "config" "user.email" "maralorn@maralorn.de"
  git "config" "user.name" "maralorn (nix-auto-updater)"
  setEnv "PATH" . toString $ Text.intercalate ":" paths
  ignoreFailure $ nix "flake" "update" "--commit-lock-file"
  changed <- LBS.null <$> (git "branch" "-r" "--contains" "HEAD" |> captureTrim)
  if changed
    then git "push" "-f" "origin" "HEAD:flake-lock-update"
    else say "No flake updates. Doing nothing."
