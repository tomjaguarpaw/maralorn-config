module Main where

import Control.Exception qualified as Exception
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Effectful (Eff, Effect, (:>))
import Effectful qualified as Eff
import Effectful.Dispatch.Dynamic qualified as Eff
import Effectful.TH (makeEffect)
import Relude
import Say (say)
import Shh ((&!>), (&>))
import Shh qualified
import System.IO qualified as IO
import Witch (into)

systems :: [Text]
systems = ["x86_64-linux", "i686-linux"]
supportedFeatures :: [Text]
supportedFeatures = ["benchmark", "big-parallel", "kvm", "nixos-test"]

data BuilderTries = FirstOf [Text] | FirstOfFinally [Text] Text | Use Text

data Ping :: Effect where
  CheckConnectivity :: Text -> Ping m Bool

makeEffect ''Ping

runWithoutConnectivity :: Eff (Ping : es) a -> Eff es a
runWithoutConnectivity = Eff.interpret $ \_ -> \case
  CheckConnectivity _ -> pure False

runWithPing :: Eff.IOE :> es => Eff (Ping : es) a -> Eff es a
runWithPing = Eff.interpret $ \_ -> \case
  CheckConnectivity host_name -> do
    liftIO $ ping `Exception.catch` \(_ :: Shh.Failure) -> pure False
   where
    ping = do
      Shh.exe ["/run/wrappers/bin/ping", into @String (sshHostToDNS host_name), "-c1", "-w1"] &> Shh.devNull &!> Shh.devNull
      pure True

sshHostToDNS :: Text -> Text
sshHostToDNS = \case
  "zeus-builder" -> "zeus.vpn.m-0.eu"
  "fluffy-builder" -> "fluffy.vpn.m-0.eu"
  "zeus-builder-local" -> "zeus.lo.m-0.eu"
  "fluffy-builder-local" -> "fluffy.lo.m-0.eu"
  host -> error [i|No dns name none for ssh host #{host}|]

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
    [ ("hera", [FirstOf ["zeus-builder"], Use "remote-builder", Use "nixbuild.net"])
    , ("apollo", [FirstOf ["zeus-builder-local", "zeus-builder"], FirstOf ["fluffy-builder-local"], Use "remote-builder", Use "nixbuild.net"])
    , ("fluffy", [FirstOf ["zeus-builder-local"], Use "remote-builder", Use "nixbuild.net"])
    , ("zeus", [Use "fluffy-builder-local", Use "remote-builder", Use "nixbuild.net"])
    ]

commaList :: [Text] -> Text
commaList = Text.intercalate ","

builderLine :: (Text, Natural, Natural) -> Text
builderLine (hostName, maxJobs, speed_factor) = [i|ssh://#{hostName} #{commaList systems} - #{maxJobs} #{speed_factor} #{commaList supportedFeatures} - -|]

testBuilders :: Ping :> es => [BuilderTries] -> Eff es [Text]
testBuilders =
  fmap catMaybes . mapM \case
    Use host -> pure $ Just host
    FirstOf hosts -> listToMaybe <$> filterM checkConnectivity hosts
    FirstOfFinally hosts fallback -> listToMaybe . (++ [fallback]) <$> filterM checkConnectivity hosts

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
  args <- getArgs
  env_host <- readFileBS "/etc/hostname" `Exception.catch` \(e :: Exception.IOException) -> pure (error (show e))
  let (host, withoutConnection) = case args of
        [] -> (Text.strip (decodeUtf8 env_host), False)
        [host'] -> (into host', False)
        [host', "--without-connection"] -> (into host', True)
        _ -> error [i|Unknown arguments: #{args}|]
      builder_tries :: Ping :> es => Eff es [Text]
      builder_tries = testBuilders $ fromMaybe (error [i|#{host} not found in builderConfigs.|]) $ Map.lookup (into host) builderConfigs
  builders <- Eff.runEff $ (if withoutConnection then runWithoutConnectivity else runWithPing) builder_tries
  (path, handle) <- IO.openTempFile "/tmp" "machines"
  TextIO.hPutStr handle (printBuilders builders)
  IO.hClose handle
  say (into path)
