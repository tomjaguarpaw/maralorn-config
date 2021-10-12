{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Aeson (
   FromJSON (..),
   eitherDecodeFileStrict',
   withObject,
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as Aeson
import Data.Aeson.Types (Value (String))
import Dialog
import Relude
import System.Process (callCommand)
import Witch

type Command = Text

instance FromJSON (Menu Command) where
   parseJSON = parseMenu "Hotkeys"
     where
      parseMenu name = withObject "mapping to menu entries" $ \object ->
         fmap (Menu name) $
            forM (Aeson.toList object) $ \(Key.toText -> key, val) ->
               case val of
                  String cmd -> pure $ Dialog.Option key cmd
                  innerObj -> Dialog.SubMenu <$> parseMenu key innerObj

main :: IO ()
main = do
   menuCommand <- eitherDecodeFileStrict' "menu.json"
   case menuCommand of
      Left err -> putStrLn err
      Right a -> do
         cmd <- runClearingHaskeline $ menu Nothing a
         maybe pass runCommand cmd

runCommand :: Command -> IO ()
runCommand = callCommand . into
