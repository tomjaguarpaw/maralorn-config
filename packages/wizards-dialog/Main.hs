{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Aeson (FromJSON (..), Value (Array, String), withObject)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.Text qualified as Text
import Data.Yaml (decodeFileEither)
import Dialog (Menu (..), MenuEntry (Option, SubMenu), menu)
import Dialog qualified
import Relude
import System.Posix.Daemon (Redirection (DevNull), runDetached)
import System.Process (callCommand)
import Witch

data Command = Run Text | Fork Text deriving (Show)

instance FromJSON (Menu Command) where
  parseJSON = parseMenu "Hotkeys"
   where
    parseList name = (fmap (Menu name) .)
      $ mapM
      $ \(Key.toText -> key, val) ->
        case val of
          String cmd -> pure $ Dialog.Option key (text2cmd cmd)
          innerObj -> Dialog.SubMenu <$> parseMenu key innerObj
    unpackMenu (Array arr) = join <$> mapM unpackMenu (reverse $ Foldable.toList arr)
    unpackMenu value = withObject "mapping to menu entries" (pure . reverse . Aeson.toList) value
    parseMenu name = parseList name <=< unpackMenu
    text2cmd t = if Text.isPrefixOf "fork " t then Fork (Text.drop 5 t) else Run t

main :: IO ()
main = do
  [menuFileName] <- getArgs
  menuCommand <- decodeFileEither menuFileName
  case menuCommand of
    Left err -> print err
    Right a -> do
      cmd <- Dialog.runClearingHaskeline $ menu Nothing a
      maybe pass runCommand cmd

runCommand :: Command -> IO ()
runCommand (Run cmd) = callCommand $ into cmd
runCommand (Fork cmd) = runDetached Nothing DevNull (callCommand $ into cmd)
