module Kass.Doc where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, withText, (.:), (.:?))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Maralude

type Docs = Map Id Doc

newtype Id = MkId {unId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

data Status = Todo | Done | Checklist | Ref Id | Tag Text
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Status where
  parseJSON = withText "status" \case
    "todo" -> pure Todo
    "done" -> pure Done
    "checklist" -> pure Checklist
    x | Just ref <- Text.stripPrefix "ref:" x -> pure $ Ref (MkId ref)
    x | Just ref <- Text.stripPrefix "tag:" x -> pure $ Tag ref
    _ -> fail "status string not matched"

instance ToJSON Status where
  toJSON =
    String . \case
      Todo -> "todo"
      Done -> "done"
      Checklist -> "checklist"
      Ref (MkId x) -> [i|ref:#{x}|]
      Tag x -> [i|tag:#{x}|]

data Doc = MkDoc
  { id :: Id
  , rev :: Maybe Text
  , deleted :: Bool
  , content :: Text
  , status :: Maybe Status
  , parent :: Maybe Id
  , tags :: Set Text
  , depends :: Set Id
  , wait :: Maybe UTCTime
  , due :: Maybe UTCTime
  , -- startdate + minutes
    scheduled :: Maybe (UTCTime, Int)
  , completedRefs :: Set Id
  , priority :: Double
  , rest :: Map Text Value
  }
  deriving stock (Eq, Ord, Show, Generic)

newDoc :: Doc
newDoc =
  MkDoc
    { id = MkId ""
    , rev = Nothing
    , deleted = False
    , content = ""
    , status = Nothing
    , parent = Nothing
    , tags = mempty
    , depends = mempty
    , wait = Nothing
    , due = Nothing
    , scheduled = Nothing
    , completedRefs = mempty
    , priority = 0
    , rest = mempty
    }

reservedFields :: Set Text
reservedFields =
  from @(List Text)
    [ "_id"
    , "_rev"
    , "_deleted"
    , "content"
    , "status"
    , "parent"
    , "tags"
    , "depends"
    , "wait"
    , "due"
    , "scheduled"
    , "completed_refs"
    , "priority"
    ]

instance FromJSON Doc where
  parseJSON = withObject "Entry" \v -> do
    id' <- v .: "_id"
    rev <- v .:? "_rev"
    deleted <- fromMaybe False <$> v .:? "_deleted"
    content <- fromMaybe "" <$> v .:? "content"
    status <- v .:? "status"
    parent <- v .:? "parent"
    tags <- fromMaybe mempty <$> v .:? "tags"
    depends <- fromMaybe mempty <$> v .:? "depends"
    wait <- v .:? "wait"
    due <- v .:? "due"
    scheduled <- v .:? "scheduled"
    completedRefs <- fromMaybe mempty <$> v .:? "completed_refs"
    priority <- fromMaybe 0 <$> v .:? "priority"
    pure
      $ MkDoc
        { id = id'
        , rev
        , deleted
        , content
        , status
        , parent
        , tags
        , depends
        , wait
        , due
        , scheduled
        , completedRefs
        , priority
        , rest = Map.withoutKeys (KeyMap.toMapText v) reservedFields
        }

knownFields :: Doc -> KeyMap Value
knownFields =
  KeyMap.fromList . catMaybes . \d ->
    [ if Text.null d.id.unId then Nothing else Just ("_id", toJSON d.id)
    , d.rev <&> (("_rev",) . toJSON)
    , if d.deleted then Just ("_deleted", toJSON True) else Nothing
    , if Text.null d.content then Nothing else Just ("content", toJSON d.content)
    , d.status <&> (("status",) . toJSON)
    , d.parent <&> (("parent",) . toJSON)
    , if (null d.tags) then Nothing else Just ("tags", (toJSON d.tags))
    , if (null d.depends) then Nothing else Just ("depends", (toJSON d.depends))
    , d.wait <&> (("wait",) . toJSON)
    , d.due <&> (("due",) . toJSON)
    , d.scheduled <&> (("scheduled",) . toJSON)
    , if (null d.completedRefs) then Nothing else Just ("completed_refs", (toJSON d.completedRefs))
    , if (0 == d.priority) then Nothing else Just ("priority", (toJSON d.priority))
    ]

instance ToJSON Doc where
  toJSON = \d ->
    Object
      $ KeyMap.union
        (knownFields d)
        (KeyMap.fromMapText d.rest)
