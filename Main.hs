{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Aeson (
   FromJSON (..),
   withObject,
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as Aeson
import Data.Aeson.Types (Value (String))
import Dialog
import Relude
import System.Process (callCommand)
import Witch
import qualified Data.Text as Text
import System.Posix.Daemon (runDetached, Redirection (DevNull))
import Data.Yaml (decodeFileEither)

data Command = Run Text | Fork Text deriving (Show)

instance FromJSON (Menu Command) where
   parseJSON = parseMenu "Hotkeys"
     where
      parseMenu name = withObject "mapping to menu entries" $ \object ->
         fmap (Menu name) $
            forM (reverse $ Aeson.toList object) $ \(Key.toText -> key, val) ->
               case val of
                  String cmd -> pure $ Dialog.Option key (text2cmd cmd)
                  innerObj -> Dialog.SubMenu <$> parseMenu key innerObj
      text2cmd t = if Text.isPrefixOf "fork " t then Fork (Text.drop 5 t) else Run t


main :: IO ()
main = do
   [menuFileName] <- getArgs
   menuCommand <- decodeFileEither menuFileName
   case menuCommand of
      Left err -> print err
      Right a -> do
         cmd <- runClearingHaskeline $ menu Nothing a
         maybe pass runCommand cmd

runCommand :: Command -> IO ()
runCommand (Run cmd) = callCommand $ into cmd
runCommand (Fork cmd) = runDetached Nothing DevNull (callCommand $ into cmd)
