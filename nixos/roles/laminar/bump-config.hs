{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-missing-signatures -Wno-type-defaults -Wno-orphans #-}

import System.Environment
import           Data.String.Interpolate
import qualified          Data.Text as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Relude
import           Shh
import Say

load Absolute ["git", "niv"]
paths :: [Text]
paths =
  $$(liftTyped . mapMaybe (\x -> foldr (<|>) Nothing $ (\bin -> Text.stripSuffix [i|/#{bin}|] $ toText x) <$> ["git", "tar", "nix-prefetch-url", "gzip"])=<< runIO pathBinsAbs)

repo = "git@hera.m-0.eu:nixos-config"

main = do
  git "clone" repo "."
  setEnv "PATH" . toString $ Text.intercalate ":" paths
  ignoreFailure $ niv "update"
  changed <- (mempty /=) <$> (git "status" "--porcelain" |> captureTrim)
  when changed  $ do
    git "config" "user.email" "maralorn@maralorn.de"
    git "config" "user.name"  "maralorn (nix-auto-updater)"
    git "commit" "-am"        "Update dependencies with niv"
    git "push" "-f" "origin" "HEAD:niv-bump"
  unless changed $ say "No updates in any niv source. Doing nothing."
