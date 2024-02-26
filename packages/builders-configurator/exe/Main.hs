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
import Network.HTTP.Req qualified as Req
import Relude
import Say (say)
import System.IO qualified as IO
import Witch (into)

{-
=== Benchmark Results ===
nix run pkgs#sysbench -- cpu run

Host    events/s
----------------
Builders    9545
Zeus        5249
Hephaistos  3777
Athene      1672
Apollo      1112
Remote       767
Hera         729

-}

remoteBuilder :: Text
remoteBuilder = "remote.builder"

nixbuildDotNet :: Text
nixbuildDotNet = "nixbuild.net"

zeusBuilder :: Text
zeusBuilder = "zeus.builder"

atheneBuilder :: Text
atheneBuilder = "athene.builder"

builderInfos :: Map.Map Text Natural
builderInfos =
  Map.fromList
    [ (remoteBuilder, 32)
    , (nixbuildDotNet, 100)
    , (zeusBuilder, 12)
    , (atheneBuilder, 2)
    ]

builderConfigs :: Map.Map Text [(Text, Reachable)]
builderConfigs =
  Map.fromList
    -- don’t use athene as remote builder, it’s not beefy enough
    [ ("hera", [(zeusBuilder, Check), (atheneBuilder, Check), (remoteBuilder, Always), (nixbuildDotNet, Always)])
    , ("apollo", [(zeusBuilder, Check), (atheneBuilder, Check), (remoteBuilder, Always), (nixbuildDotNet, Always)])
    , ("hephaistos", [(zeusBuilder, Check), (atheneBuilder, Check), (remoteBuilder, Always), (nixbuildDotNet, Always)])
    , ("athene", [(zeusBuilder, Check), (remoteBuilder, Always), (nixbuildDotNet, Always)])
    , ("zeus", [(remoteBuilder, Always), (atheneBuilder, Check), (nixbuildDotNet, Always)])
    ]

systems :: [Text]
systems = ["x86_64-linux", "i686-linux"]

supportedFeatures :: [Text]
supportedFeatures = ["benchmark", "big-parallel", "kvm", "nixos-test"]

data Reachable = Always | Check

data Ping :: Effect where
  CheckConnectivity :: Text -> Ping m Bool

makeEffect ''Ping

runWithoutConnectivity :: Eff (Ping : es) a -> Eff es a
runWithoutConnectivity = Eff.interpret $ \_ -> \case
  CheckConnectivity _ -> pure False

runWithPing :: (Eff.IOE :> es) => Eff (Ping : es) a -> Eff es a
runWithPing = Eff.interpret $ \_ -> \case
  CheckConnectivity host_name -> liftIO $ Exception.handle (\(_ :: SomeException) -> pure False) do
    let reqUrl = Req.http (sshHostToDNS host_name)
    response <- (Req.runReq Req.defaultHttpConfig $ Req.req Req.GET reqUrl Req.NoReqBody Req.lbsResponse (Req.responseTimeout 500_000))
    let status = Req.responseStatusCode response
    pure $ status >= 200 && status < 300

sshHostToDNS :: Text -> Text
sshHostToDNS = \case
  b | b == zeusBuilder -> "zeus.vpn.m-0.eu"
  b | b == atheneBuilder -> "athene.vpn.m-0.eu"
  host -> error [i|No dns name none for ssh host #{host}|]

commaList :: [Text] -> Text
commaList = Text.intercalate ","

builderLine :: (Text, Natural, Natural) -> Text
builderLine (hostName, maxJobs, speed_factor) = [i|ssh-ng://#{hostName} #{commaList systems} - #{maxJobs} #{speed_factor} #{commaList supportedFeatures} - -|]

testBuilders :: (Ping :> es) => [(Text, Reachable)] -> Eff es [Text]
testBuilders =
  fmap (fmap fst) . filterM \case
    (_, Always) -> pure True
    (host, Check) -> checkConnectivity host

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
  let (host, withoutConnection, allow_empty) = case args of
        [] -> (Text.strip (decodeUtf8 env_host), False, True)
        [host'] -> (into host', False, True)
        [host', "--force"] -> (into host', False, False)
        [host', "--without-connection"] -> (into host', True, False)
        _ -> error [i|Unknown arguments: #{args}|]
      builder_tries :: (Ping :> es) => Eff es [Text]
      builder_tries = testBuilders $ fromMaybe (error [i|#{host} not found in builderConfigs.|]) $ Map.lookup (into host) builderConfigs
  builders <- if allow_empty && host `elem` ["zeus", "hephaistos"] then pure [] else Eff.runEff $ (if withoutConnection then runWithoutConnectivity else runWithPing) builder_tries
  (path, handle) <- IO.openTempFile "/tmp" "machines"
  TextIO.hPutStr handle (printBuilders builders)
  IO.hClose handle
  say (into path)
