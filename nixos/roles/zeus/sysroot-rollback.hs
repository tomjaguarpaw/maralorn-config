{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Exception.Safe qualified as Exception
import Relude
import Say (say)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.IO qualified as Encoding

dirsToKeep :: [String]
dirsToKeep = ["nix", "disk"]

main :: IO ()
main = do
  Encoding.hSetEncoding stdout Encoding.utf8
  say "Deleting everyting in / but /nix and /disk …"
  entries <- Directory.listDirectory "/sysroot"
  let toDelete = filter (`notElem` dirsToKeep) entries
  forM_ toDelete \entry -> do
    say $ "Deleting /sysroot/" <> toText entry <> " …"
    Directory.removePathForcibly ("/sysroot" </> entry) `Exception.catchAny` \e ->
      say $ "Failed to remove /sysroot/" <> toText entry <> ": " <> toText (displayException e)
  say "Rollback complete."
