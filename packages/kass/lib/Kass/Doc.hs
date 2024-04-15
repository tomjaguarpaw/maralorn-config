module Kass.Doc where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, (.:), (.:?))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Maralude

type Docs = Map Id Doc

newtype Id = MkId {unId :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Doc = MkDoc
  { id :: Id
  , rev :: Maybe Text
  , deleted :: Bool
  , content :: Text
  , parent :: Maybe Id
  , tags :: Set Text
  , rest :: Map Text Value
  }
  deriving stock (Eq, Ord, Show, Generic)

reservedFields :: Set Text
reservedFields = from @(List Text) ["_id", "_rev", "_deleted", "content", "parent", "tags"]

instance FromJSON Doc where
  parseJSON = withObject "Entry" \v -> do
    id' <- v .: "_id"
    rev <- v .:? "_rev"
    deleted <- fromMaybe False <$> v .:? "_deleted"
    content <- fromMaybe "" <$> v .:? "content"
    parent <- v .:? "parent"
    tags <- fromMaybe mempty <$> v .:? "tags"
    pure
      $ MkDoc
        { id = id'
        , rev
        , deleted
        , content
        , parent
        , tags
        , rest = Map.withoutKeys (KeyMap.toMapText v) reservedFields
        }

knownFields :: Doc -> KeyMap Value
knownFields = \d ->
  KeyMap.fromList
    . catMaybes
    $ [ Just ("_id", toJSON d.id)
      , d.rev <&> (("_rev",) . toJSON)
      , if d.deleted then Just ("_deleted", toJSON True) else Nothing
      , if Text.null d.content then Nothing else Just ("content", toJSON d.content)
      , d.parent <&> (("parent",) . toJSON)
      , if (null d.tags) then Nothing else Just ("tags", (toJSON d.tags))
      ]

instance ToJSON Doc where
  toJSON = \d ->
    Object
      $ KeyMap.union
        (knownFields d)
        (KeyMap.fromMapText d.rest)
