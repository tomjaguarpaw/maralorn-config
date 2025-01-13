{-# LANGUAGE DerivingStrategies #-}

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
Hephaistos  5902
Zeus        5249
Athene      1672
Apollo      1112
Remote       767
Hera         729

-}

newtype Builder = MkBuilder Text
  deriving newtype (IsString, Eq, Ord, Show)

builder :: Builder -> Text
builder = coerce

newtype BuilderGroup = MkBuilderGroup (Set Builder)
  deriving newtype (Eq, Ord, Show)

builderList :: BuilderGroup -> [Builder]
builderList = toList @Set . coerce

remoteBuilder :: Builder
remoteBuilder = "remote.builder"

remoteGroup :: BuilderGroup
remoteGroup = MkBuilderGroup (one remoteBuilder)

nixbuildDotNet :: Builder
nixbuildDotNet = "nixbuild.net"

nixbuildGroup :: BuilderGroup
nixbuildGroup = MkBuilderGroup (one nixbuildDotNet)

zeusBuilder :: Builder
zeusBuilder = "zeus.builder"

zeusGroup :: BuilderGroup
zeusGroup = MkBuilderGroup (one zeusBuilder)

cccda1 :: Builder
cccda1 = "build1.darmstadt.ccc.de"

cccda2 :: Builder
cccda2 = "build2.darmstadt.ccc.de"

cccda3 :: Builder
cccda3 = "build3.darmstadt.ccc.de"

cccda4 :: Builder
cccda4 = "build4.darmstadt.ccc.de"

cccdaGroup :: BuilderGroup
cccdaGroup = MkBuilderGroup (fromList [cccda1, cccda2, cccda3, cccda4])

builderInfos :: Map.Map Builder Natural
builderInfos =
  Map.fromList
    [ (cccda1, 10)
    , (cccda2, 10)
    , (cccda3, 10)
    , (cccda4, 5)
    , (remoteBuilder, 32)
    , (nixbuildDotNet, 100)
    , (zeusBuilder, 6)
    ]

mkConfig :: Bool -> [(BuilderGroup, Reachable)]
mkConfig isZeus =
  ([(zeusGroup, Check) | not isZeus])
    <> [(cccdaGroup, Always), (remoteGroup, Always), (nixbuildGroup, Always)]

builderConfigs :: Map.Map Text [(BuilderGroup, Reachable)]
builderConfigs =
  Map.fromList
    [ ("hera", mkConfig False)
    , ("hephaistos", mkConfig False)
    , ("athene", mkConfig False)
    , ("zeus", mkConfig True)
    ]

systems :: [Text]
systems = ["x86_64-linux", "i686-linux"]

supportedFeatures :: [Text]
supportedFeatures = ["benchmark", "big-parallel", "kvm", "nixos-test"]

data Reachable = Always | Check

data Ping :: Effect where
  CheckConnectivity :: Builder -> Ping m Bool

makeEffect ''Ping

runWithoutConnectivity :: Eff (Ping : es) a -> Eff es a
runWithoutConnectivity = Eff.interpret $ \_ -> \case
  CheckConnectivity _ -> pure False

runWithPing :: Eff.IOE :> es => Eff (Ping : es) a -> Eff es a
runWithPing = Eff.interpret $ \_ -> \case
  CheckConnectivity host_name -> liftIO $ Exception.handle (\(_ :: SomeException) -> pure False) do
    let reqUrl = Req.http case builder host_name of
          "zeus.builder" -> "zeus.vpn.m-0.eu"
          x -> x
    response <-
      (Req.runReq Req.defaultHttpConfig $ Req.req Req.GET reqUrl Req.NoReqBody Req.lbsResponse (Req.responseTimeout 500_000))
    let status = Req.responseStatusCode response
    pure $ status >= 200 && status < 300

commaList :: [Text] -> Text
commaList = Text.intercalate ","

builderLine :: Builder -> Natural -> Natural -> Text
builderLine hostName maxJobs speed_factor =
  [i|ssh-ng://#{builder hostName} #{commaList systems} - #{maxJobs} #{speed_factor} #{commaList supportedFeatures} - -|]

testBuilders :: Ping :> es => [(BuilderGroup, Reachable)] -> Eff es [BuilderGroup]
testBuilders =
  fmap (fmap fst) . filterM \case
    (_, Always) -> pure True
    (hosts, Check) -> andM (checkConnectivity <$> builderList hosts)

printBuilders :: [BuilderGroup] -> Text
printBuilders = Text.unlines . (uncurry flatten_group <=< Foldable.foldr' folder [])
 where
  flatten_group :: [(Builder, Natural)] -> Natural -> [Text]
  flatten_group = \builder_group speed_factor ->
    builder_group <&> \(host, max_jobs) -> builderLine host max_jobs speed_factor
  folder :: BuilderGroup -> [([(Builder, Natural)], Natural)] -> [([(Builder, Natural)], Natural)]
  folder builder_group collected_lines = next_entry : collected_lines
   where
    next_entry = (group_jobs, maybe 1 (\(_, x) -> max_max_jobs * x) (viaNonEmpty head collected_lines))
    group_jobs = builderList builder_group <&> \name -> (name, max_jobs name)
    max_max_jobs = Foldable.maximum (1 : (snd <$> group_jobs))
    max_jobs name = fromMaybe (error [i|#{name} not found in builderInfos.|]) $ Map.lookup name builderInfos

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
      builder_tries :: Ping :> es => Eff es [BuilderGroup]
      builder_tries = testBuilders $ fromMaybe (error [i|#{host} not found in builderConfigs.|]) $ Map.lookup (into host) builderConfigs
  builders <-
    if allow_empty && host `elem` ["zeus", "hephaistos"]
      then pure []
      else Eff.runEff $ (if withoutConnection then runWithoutConnectivity else runWithPing) builder_tries
  (path, handle) <- IO.openTempFile "/tmp" "machines"
  TextIO.hPutStr handle (printBuilders builders)
  IO.hClose handle
  say (into path)
