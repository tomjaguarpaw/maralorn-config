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

import Data.String.Interpolate
import Data.Text qualified as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude
import Say
import Shh
import System.Environment

load Absolute ["git", "niv"]
paths :: [Text]
paths =
  $$( bindCode (runIO pathBinsAbs) \rawPaths ->
        let wantedPaths :: [Text] = mapMaybe (\x -> foldr (<|>) Nothing $ (\bin -> Text.stripSuffix [i|/#{bin}|] $ toText x) <$> ["git", "tar", "nix-prefetch-url", "gzip", "ssh"]) rawPaths
         in liftTyped wantedPaths
    )

repo = "git@hera.m-0.eu:nixos-config"

main = do
  git "clone" repo "."
  setEnv "PATH" . toString $ Text.intercalate ":" paths
  ignoreFailure $ niv "update"
  changed <- (mempty /=) <$> (git "status" "--porcelain" |> captureTrim)
  when changed $ do
    git "config" "user.email" "maralorn@maralorn.de"
    git "config" "user.name" "maralorn (nix-auto-updater)"
    git "commit" "-am" "Update dependencies with niv"
    git "push" "-f" "origin" "HEAD:niv-bump"
  unless changed $ say "No updates in any niv source. Doing nothing."
