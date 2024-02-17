{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Exception.Safe (catchAny)
import Relude
import Say (say)
import System.Directory (listDirectory, removePathForcibly)
import System.FilePath ((</>))

dirsToKeep :: [String]
dirsToKeep = ["nix", "disk"]

main :: IO ()
main = do
  say "Rolling back / by clearing everything but /nix and /disk ..."
  entries <- listDirectory "/sysroot"
  let toDelete = filter (`notElem` dirsToKeep) entries
  forM_ toDelete \entry -> do
    removePathForcibly ("/sysroot" </> entry) `catchAny` \e ->
      say $ "Failed to remove /sysroot/" <> toText entry <> ": " <> toText (displayException e)
  say "Rollback complete."
