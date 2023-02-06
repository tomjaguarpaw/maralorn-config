{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Relude
import Say (say)
import System.IO qualified as IO
import Witch (into)

systems :: [Text]
systems = ["x86_64-linux", "i686-linux"]
supportedFeatures :: [Text]
supportedFeatures = ["benchmark", "big-parallel", "kvm", "nixos-test"]

data BuilderTries = FirstOf [Text] | FirstOfFinally [Text] Text | Use Text

builderInfos :: Map.Map Text Natural
builderInfos =
  Map.fromList
    [ ("remote-builder", 32)
    , ("nixbuild.net", 100)
    , ("zeus-builder", 12)
    , ("fluffy-builder", 2)
    , ("zeus-builder-local", 12)
    , ("fluffy-builder-local", 2)
    ]
builderConfigs :: Map.Map Text [BuilderTries]
builderConfigs =
  Map.fromList
    [ ("hera", [FirstOf ["zeus-builder"], Use "fluffy-builder", Use "remote-builder", Use "nixbuild.net"])
    , ("apollo", [FirstOf ["zeus-builder-local", "zeus-builder"], FirstOfFinally ["fluffy-builder-local"] "fluffy-builder", Use "remote-builder", Use "nixbuild.net"])
    , ("fluffy", [FirstOf ["zeus-builder-local"], Use "fluffy-builder-local", Use "remote-builder", Use "nixbuild.net"])
    , ("zeus", [Use "zeus-builder-local", Use "fluffy-builder-local", Use "remote-builder", Use "nixbuild.net"])
    ]

commaList :: [Text] -> Text
commaList = Text.intercalate ","

builderLine :: (Text, Natural, Natural) -> Text
builderLine (hostName, maxJobs, speed_factor) = [i|ssh://#{hostName} #{commaList systems} - #{maxJobs} #{speed_factor} #{commaList supportedFeatures} - -|]

testBuilders :: [BuilderTries] -> IO [Text]
testBuilders =
  pure
    . mapMaybe
      ( \case
          Use x -> Just x
          FirstOf x -> listToMaybe x
          FirstOfFinally x y -> listToMaybe x <|> pure y
      )

printBuilders :: [Text] -> Text
printBuilders = Text.unlines . fmap builderLine . Foldable.foldr' folder []
 where
  folder :: Text -> [(Text, Natural, Natural)] -> [(Text, Natural, Natural)]
  folder builder_name collected_lines = next_entry : collected_lines
   where
    next_entry = (builder_name, max_jobs, maybe 1 (\(_, _, x) -> max_jobs * x) (viaNonEmpty head collected_lines))
    max_jobs = fromMaybe (error [i|#{builder_name} not found in builderInfos.|]) $ Map.lookup builder_name builderInfos

main :: IO ()
main = do
  (host : args) <- getArgs
  builders <- testBuilders $ fromMaybe (error [i|#{host} not found in builderConfigs.|]) $ Map.lookup (into host) builderConfigs
  (path, handle) <- IO.openTempFile "/tmp" "machines"
  TextIO.hPutStr handle (printBuilders builders)
  IO.hClose handle
  say (into path)
