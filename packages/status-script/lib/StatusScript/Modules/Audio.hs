module StatusScript.Modules.Audio (audioUpdateEvent, audioInfos) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Schema (get, schema)
import Data.Aeson.Schema qualified as Schema
import Data.ByteString.Lazy.Char8 qualified as LazyByteStringChar
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Text qualified as Text
import Reflex
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Relude hiding (get, mapMaybe)
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["pw-dump"]

missingExecutables :: IO [FilePath]
audioUpdateEvent :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Aeson.Value])
audioUpdateEvent = \env -> do
  CommandUtil.reportMissing missingExecutables
  line <- ReflexUtil.processLines env (pw_dump "-m")
  R.mapMaybe snd . R.updated <$> R.foldDyn foldJsonLines ([], Nothing) line

foldJsonLines :: ByteString -> ([ByteString], Maybe [Aeson.Value]) -> ([ByteString], Maybe [Aeson.Value])
foldJsonLines = \next_line -> \case
  (unparsed_lines, _)
    | next_line == "]" ->
        ( []
        , (next_line : unparsed_lines)
            & reverse
            & fmap fromStrict
            & LazyByteStringChar.unlines
            & Aeson.decode
        )
  (unparsed_lines, _) -> (next_line : unparsed_lines, Nothing)

data AudioClient = MkAudioClient
  { name :: Text
  , volume :: Double
  , mute :: Bool
  }
  deriving stock (Generic, Eq)
  deriving anyclass (Aeson.ToJSON)

data AudioEndPointType = Source | Sink
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

data AudioEndPoint = MkAudioEndPoint
  { name :: Text
  , volume :: Double
  , mute :: Bool
  , icons :: [Char]
  , clients :: [AudioClient]
  }
  deriving stock (Generic, Eq)
  deriving anyclass (Aeson.ToJSON)

type Value = Aeson.Value

type PipeWireObject =
  [schema|{
    id: Int,
    type: Text,
    info: Value,
    metadata: Maybe List Value
  }|]

type PipeWireObjectDelete =
  [schema| {
  id: Int,
  info: Maybe Bool
} |]

fromJSON :: Aeson.FromJSON a => Value -> Maybe a
fromJSON =
  Aeson.fromJSON >>> \case
    Aeson.Success x -> Just x
    _ -> Nothing

extractJSON :: Aeson.FromJSON a => [Aeson.Key] -> Value -> Maybe a
extractJSON = \path obj -> foldlM (\obj' key -> extractOneJSON key obj') obj path >>= fromJSON

extractOneJSON :: Aeson.Key -> Value -> Maybe Value
extractOneJSON = \key -> fromJSON @(Aeson.Object) >=> KeyMap.lookup key

aliases :: [(Text, Text)]
aliases =
  [ ("Q30", "Overears")
  , ("USB PnP Audio Device", "USB Mic")
  , ("athene", "Boxen")
  , ("HDA ATI HDMI", "Monitor")
  , ("Audio Controller HDMI", "Monitor")
  , ("Audio Controller Speaker", "Intern")
  , ("Audio Controller Digital Microphone", "Intern")
  , ("DP Audio Digital Stereo (HDMI)", "TV")
  , ("DP Audio Digital Stereo (HDMI 4)", "Monitor")
  , ("Headphones", "Kopfhörer Klinke")
  , ("Built-in Audio", "Intern")
  , ("Dock", "Dock")
  , ("Dummy Output", "No Output")
  ]

mkInfos :: IntMap (Schema.Object PipeWireObject) -> [AudioEndPoint]
mkInfos = \objects ->
  let
    get_info :: Aeson.FromJSON a => Int -> [Aeson.Key] -> Maybe a
    get_info = \id' path ->
      IntMap.lookup id' objects
        >>= extractJSON path . [get|.info|]
    get_name :: Int -> Text
    get_name id' =
      let raw = get_info id' ["props", "node.description"] & fromMaybe "unknown"
       in find (fst >>> (`Text.isInfixOf` raw)) aliases
            & maybe raw snd
    find_volume :: Int -> Maybe Double
    find_volume = \id' ->
      get_info id' ["params", "Props"]
        <&> mapMaybe (extractJSON ["channelVolumes"])
        >>= viaNonEmpty head
        <&> ( Foldable.maximum
                -- Factors found at https://gitlab.freedesktop.org/pulseaudio/pulseaudio/-/blob/master/src/pulse/volume.c#L260
                -- explained at https://www.robotplanet.dk/audio/audio_gui_design/
                >>> (/ 3.162278)
                >>> (** (1 / 3))
                >>> (* 100)
            )
    find_mute :: Int -> Maybe Bool
    find_mute = \id' -> get_info id' ["params", "Props"] <&> mapMaybe (extractJSON ["mute"]) >>= viaNonEmpty head
    defaults :: [Text]
    defaults =
      objects
        & toList
        & filter (\obj -> [get| obj.type |] == "PipeWire:Interface:Metadata")
        & mapMaybe [get|.metadata|]
        & join
        & filter (extractJSON ["key"] >>> (`elem` [Just "default.audio.sink", Just "default.audio.source"]))
        & mapMaybe (extractJSON ["value", "name"])
    links =
      objects
        & toList
        & filter (\obj -> [get| obj.type |] == "PipeWire:Interface:Link")
        & mapMaybe
          ((\info -> (,) <$> extractJSON ["input-node-id"] info <*> extractJSON ["output-node-id"] info) . [get|.info|])
        & fmap (second IntSet.singleton)
        & IntMap.fromListWith (<>)
    endpoints =
      objects
        & toList
        & filter (\obj -> [get| obj.type |] == "PipeWire:Interface:Node")
        & filter ([get|.info|] >>> extractJSON ["props", "media.class"] >>> (`elem` [Just "Audio/Sink", Just "Audio/Source"]))
        & fmap [get|.id|]
   in
    endpoints
      <&> ( \endpoint ->
              let mute = fromMaybe False (find_mute endpoint)
               in MkAudioEndPoint
                    { name = get_name endpoint
                    , volume = fromMaybe 0 (find_volume endpoint)
                    , mute
                    , -- TODO: Add mute and default icons
                      icons =
                        [ case (get_info endpoint ["props", "media.class"] == Just "Audio/Source", mute) of
                            (True, False) -> toEnum 983916 -- nf-md-microphone
                            (True, True) -> toEnum 983917 -- nf-md-microphone_off
                            (False, False) -> toEnum 984446 -- nf-md-volume_high
                            (False, True) -> toEnum 984449 -- nf-md-volume_off
                        ]
                          <> [ toEnum 0xf0970 -- nf-md-headphones_bluetooth
                             | get_info endpoint ["props", "api.bluez5.profile"] == Just "headset-head-unit"
                             ]
                          <> [ toEnum 0xf00b0 -- nf-md-bluetooth_audio
                             | get_info endpoint ["props", "api.bluez5.profile"] == Just "a2dp-sink"
                             ]
                          <> [ toEnum 0xf05e1 -- nf-md-check_circle_outline
                             | get_info endpoint ["props", "node.name"] `elem` (defaults <&> Just)
                             ]
                    , clients =
                        IntMap.lookup endpoint links
                          & maybe [] IntSet.toList
                          & mapMaybe
                            ( \source ->
                                get_info source ["props", "application.name"]
                                  <&> \source_name ->
                                    MkAudioClient
                                      { name = source_name
                                      , volume = fromMaybe 0 (find_volume source)
                                      , mute = fromMaybe False (find_mute source)
                                      }
                            )
                    }
          )
      & filter \endpoint -> length endpoint.clients + length endpoint.icons > 1

audioInfos :: R.MonadHeadlessApp t m => R.Event t [Aeson.Value] -> m (R.Event t [AudioEndPoint])
audioInfos =
  R.foldDyn foldPipeWireEvents mempty
    >=> fmap updated
    . R.holdUniqDyn
    . fmap mkInfos

foldPipeWireEvents :: [Aeson.Value] -> IntMap (Schema.Object PipeWireObject) -> IntMap (Schema.Object PipeWireObject)
foldPipeWireEvents = \events start_objects -> foldl' (flip foldPipeWireEvent) start_objects events

foldPipeWireEvent :: Aeson.Value -> IntMap (Schema.Object PipeWireObject) -> IntMap (Schema.Object PipeWireObject)
foldPipeWireEvent = \obj objects ->
  case (fromJSON @(Schema.Object PipeWireObject) obj, fromJSON @(Schema.Object PipeWireObjectDelete) obj) of
    (Just object, _) -> IntMap.insert [get| object.id |] object objects
    (_, Just deletion) -> IntMap.delete [get| deletion.id |] objects
    _ -> objects
