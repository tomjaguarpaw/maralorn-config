{-# LANGUAGE DataKinds #-}

module StatusScript.Modules.Audio (audioUpdateEvent, audioInfos) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Schema (get, schema)
import Data.Aeson.Schema qualified as Schema
import Data.ByteString.Lazy.Char8 qualified as LazyByteStringChar
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Maralorn.Prelude hiding (get)
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["pw-dump"]
missingExecutables :: IO [FilePath]
audioUpdateEvent :: R.MonadHeadlessApp t m => m (R.Event t [Aeson.Value])
audioUpdateEvent = do
  CommandUtil.reportMissing missingExecutables
  line <- ReflexUtil.processLines (pw_dump "-m")
  R.foldDyn foldJsonLines ([], Nothing) line <&> R.updated % R.mapMaybe snd

foldJsonLines :: ByteString -> ([ByteString], Maybe [Aeson.Value]) -> ([ByteString], Maybe [Aeson.Value])
foldJsonLines = \next_line -> \case
  (unparsed_lines, _)
    | next_line == "]" ->
        ( []
        , (next_line : unparsed_lines)
            & reverse
            %> fromStrict
            % LazyByteStringChar.unlines
            % Aeson.decode
        )
  (unparsed_lines, _) -> (next_line : unparsed_lines, Nothing)

data AudioClient = MkAudioClient
  { name :: Text
  , volume :: Double
  , mute :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

data AudioEndPointType = Source | Sink
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

data AudioEndPoint = MkAudioEndPoint
  { name :: Text
  , volume :: Double
  , mute :: Bool
  , isDefault :: Bool
  , bTMode :: Maybe Text
  , audioType :: AudioEndPointType
  , clients :: [AudioClient]
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

type Value = Aeson.Value

type PipeWireObject =
  [schema|{
    id: Int,
    type: Text,
    info: Value
  }|]

type LinkInfo =
  [schema|{
      output-node-id: Int,
      input-node-id: Int,
  }|]

type NodeInfo =
  [schema|{
    props: Value
  }|]

type PipeWireObjectDelete =
  [schema| {
  id: Int,
  info: Maybe Bool
} |]

fromJSON :: Aeson.FromJSON a => Value -> Maybe a
fromJSON =
  Aeson.fromJSON % \case
    Aeson.Success x -> Just x
    _ -> Nothing

audioInfos :: R.MonadHeadlessApp t m => R.Event t [Aeson.Value] -> m (R.Event t [AudioEndPoint])
audioInfos = \trigger_event ->
  R.foldDyn foldPipeWireEvents mempty trigger_event
    <&> R.updated
    %> \objects ->
      objects
        & toList
        % filter (\obj -> [get| obj.type |] == "PipeWire:Interface:Link")
        %> [get|.info|]
        % mapMaybe (fromJSON @(Schema.Object LinkInfo))
        %> (\link -> ([get|link.input-node-id|], IntSet.singleton [get|link.output-node-id|]))
        % IntMap.fromListWith (<>)
        % IntMap.toList
        % mapMaybe \(sink, sources) ->
          IntMap.lookup sink objects
            >>= ([get|.info|] % fromJSON @(Schema.Object NodeInfo))
            >>= ([get|.props|] % fromJSON @(Aeson.Object))
            >>= (KeyMap.lookup "node.description")
            >>= fromJSON @Text
            <&> \name ->
              MkAudioEndPoint
                { name = name
                , volume = 1
                , mute = False
                , isDefault = False
                , bTMode = Nothing
                , audioType = Source
                , clients =
                    IntSet.toList sources
                      & mapMaybe
                        ( \source ->
                            IntMap.lookup source objects
                              >>= ([get|.info|] % fromJSON @(Schema.Object NodeInfo))
                              >>= ([get|.props|] % fromJSON @(Aeson.Object))
                              >>= (KeyMap.lookup "application.name")
                              >>= fromJSON @Text
                              <&> \source_name ->
                                MkAudioClient
                                  { name = source_name
                                  , volume = 0
                                  , mute = False
                                  }
                        )
                }

foldPipeWireEvents :: [Aeson.Value] -> IntMap (Schema.Object PipeWireObject) -> IntMap (Schema.Object PipeWireObject)
foldPipeWireEvents = \events start_objects -> foldl' (flip foldPipeWireEvent) start_objects events

foldPipeWireEvent :: Aeson.Value -> IntMap (Schema.Object PipeWireObject) -> IntMap (Schema.Object PipeWireObject)
foldPipeWireEvent = \obj objects ->
  case (fromJSON @(Schema.Object PipeWireObject) obj, fromJSON @(Schema.Object PipeWireObjectDelete) obj) of
    (Just object, _) -> IntMap.insert [get| object.id |] object objects
    (_, Just deletion) -> IntMap.delete [get| deletion.id |] objects
    _ -> objects
