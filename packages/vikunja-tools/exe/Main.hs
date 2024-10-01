module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict (modifyM)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode', encode)
import Data.Aeson.Optics
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Network.Wreq hiding (get)
import Network.Wreq qualified as Wreq hiding (get)
import Optics
import Relude

url :: Text
url = "https://todo.darmstadt.ccc.de/api/v1"

inboxLabel
  , parentLabel
  , blockedLabel
  , maybeLabel
  , shouldLabel
  , mustLabel
  , wantLabel
  , waitingLabel
  , categoryLabel
    :: Label
inboxLabel = Label 11
parentLabel = Label 12
blockedLabel = Label 13
shouldLabel = Label 29
mustLabel = Label 28
wantLabel = Label 27
maybeLabel = Label 14
waitingLabel = Label 26
categoryLabel = Label 15

autoLabels :: Set Label
autoLabels = Set.fromList [inboxLabel, wantLabel, mustLabel, shouldLabel]

nonListLabels :: Set Label
nonListLabels = autoLabels <> Set.fromList [parentLabel, blockedLabel, categoryLabel]

maybeBucket, waitingBucket, inboxBucket, inactiveBucket, backlogBucket :: Int
maybeBucket = 52
waitingBucket = 53
inboxBucket = 55
inactiveBucket = 56
backlogBucket = 42

wantColor, mustColor, shouldColor :: Text
wantColor = "1a5fb4"
mustColor = "ffbe6f"
shouldColor = ""

getToken :: IO Text
getToken = Text.strip . decodeUtf8 <$> readFileBS "/run/agenix/vikunja-token"

newtype Label = Label
  { id :: Int
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data Task = Task
  { id :: Int
  , created :: Text
  , project_id :: Int
  , priority :: Int
  , bucket_id :: Int
  , labels :: Maybe (Set Label)
  , related_tasks :: RelatedTasks
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data RelatedTasks = RelatedTasks
  { subtask :: Maybe (Set RelatedTask)
  , blocked :: Maybe (Set RelatedTask)
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

newtype RelatedTask = RelatedTask
  { done :: Bool
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data SetLabel = SetLabel
  { created :: Text
  , label_id :: Int
  }
  deriving anyclass (ToJSON)
  deriving stock (Generic, Show, Eq, Ord)

ensureLabel :: Options -> Task -> Label -> Bool -> IO Task
ensureLabel opts task label should_exist = do
  when (new_task /= task) $ do
    putTextLn [i|#{if should_exist then "setting" else "unsetting" :: Text} #{label} for #{task}|]
    void $
      if should_exist
        then Wreq.putWith opts [i|#{url}/tasks/#{task_id}/labels|] (encode $ SetLabel task.created label.id)
        else Wreq.deleteWith opts [i|#{url}/tasks/#{task_id}/labels/#{label_id}|]
  pure new_task
 where
  task_id = task.id
  label_id = label.id
  new_task = task & #labels % non mempty %~ (if should_exist then Set.insert else Set.delete) label

ensureBucket :: Int -> Value -> Value
ensureBucket bucket_id = set (key "bucket_id" % _Integral) bucket_id

ensureColor :: Text -> Value -> Value
ensureColor color = set (key "hex_color" % _String) color

ensureChange :: Options -> Task -> (Value -> Value) -> Value -> IO ()
ensureChange opts task f task_js = when (new_task /= task_js) $ do
  putTextLn [i|Updating #{task} to #{new_task}|]
  void $ Wreq.postWith opts [i|#{url}/tasks/#{task_id}|] (encode new_task)
 where
  task_id = task.id
  new_task = f task_js

chooseBucket :: Task -> Int
chooseBucket t
  | inboxLabel `Set.member` task_labels = inboxBucket
  | Set.isSubsetOf task_labels nonListLabels = inactiveBucket
  | t.bucket_id `elem` [inboxBucket, inactiveBucket] = backlogBucket
  | otherwise = t.bucket_id
 where
  task_labels = fromMaybe mempty t.labels

chooseColor :: Task -> Text
chooseColor t = case t.priority of
  1 -> wantColor
  2 -> mustColor
  _ -> shouldColor

update :: Options -> IO ()
update opts = do
  response <- Wreq.getWith opts [i|#{url}/tasks/all|]
  tasks <- either (error . toText) pure $ taskAndJS $ response ^. lensVL responseBody
  putTextLn [i|Checking #{length tasks} tasks.|]
  forM_ tasks $ uncurry \value -> runStateT do
    modifyM \t -> ensureLabel opts t shouldLabel (0 == t.priority)
    modifyM \t -> ensureLabel opts t wantLabel (1 == t.priority)
    modifyM \t -> ensureLabel opts t mustLabel (2 == t.priority)
    modifyM \t -> ensureLabel opts t waitingLabel (waitingBucket == t.bucket_id)
    modifyM \t -> ensureLabel opts t maybeLabel (maybeBucket == t.bucket_id)
    modifyM \t -> ensureLabel opts t blockedLabel (not (all (.done) (fromMaybe mempty t.related_tasks.blocked)))
    modifyM \t -> ensureLabel opts t parentLabel (not (all (.done) (fromMaybe mempty t.related_tasks.subtask)))
    modifyM \t -> ensureLabel opts t inboxLabel (Set.isSubsetOf (fromMaybe mempty t.labels) autoLabels)
    get >>= \t -> lift $ ensureChange opts t (ensureBucket (chooseBucket t) . ensureColor (chooseColor t)) value

main :: IO ()
main = do
  token <- getToken
  let opts =
        defaults
          & lensVL (header "Authorization") .~ [[i|Bearer #{token}|]]
          & lensVL (header "Content-Type") .~ ["application/json"]
  forever do
    update opts
    threadDelay 20_000_000
    forM_ [1 :: Int .. 15] $ \_ -> do
      update $
        opts
          & lensVL (param "filter_by") .~ ["updated"]
          & lensVL (param "filter_value") .~ ["now-125m"]
          & lensVL (param "filter_comparator") .~ ["greater"]
      threadDelay 20_000_000

taskAndJS :: LByteString -> Either String [(Value, Task)]
taskAndJS s = do
  js <- eitherDecode' s
  tasks <- eitherDecode' s
  pure $ zip js tasks
