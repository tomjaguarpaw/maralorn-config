module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict (modifyM)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode', encode)
import Data.Aeson.Optics
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Network.Wreq hiding (get)
import Network.Wreq qualified as Wreq hiding (get)
import Optics
import Relude

url :: Text
url = "https://todo.darmstadt.ccc.de/api/v1"

inboxLabel, parentLabel, categoryLabel, unsortedLabel :: Label
inboxLabel = Label 11
parentLabel = Label 12
categoryLabel = Label 15
unsortedLabel = Label 30

autoLabels :: Set Label
autoLabels = Set.fromList [inboxLabel, unsortedLabel]

nonListLabels :: Set Label
nonListLabels = autoLabels <> Set.fromList [parentLabel, categoryLabel]

maybeBucket, awaitingBucket, inboxBucket, waitingBucket, projectBucket, backlogBucket :: Bucket
maybeBucket = 52
awaitingBucket = 53
inboxBucket = 55
waitingBucket = 56
projectBucket = 58
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

newtype Bucket = Bucket Int
  deriving newtype (Num, FromJSON, ToJSON, Show, Eq, Ord)

data Task = Task
  { id :: Int
  , created :: Text
  , project_id :: Int
  , priority :: Int
  , bucket_id :: Bucket
  , labels :: Maybe (Set Label)
  , related_tasks :: RelatedTasks
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data RelatedTasks = RelatedTasks
  { subtask :: Maybe (Set RelatedTask)
  , blocked :: Maybe (Set RelatedTask)
  , parenttask :: Maybe (Set RelatedTask)
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data RelatedTask = RelatedTask
  { done :: Bool
  , bucket_id :: Bucket
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

ensureBucket :: Bucket -> Value -> Value
ensureBucket bucket_id = set (key "bucket_id" % _Integral) (coerce bucket_id :: Int)

ensureColor :: Text -> Value -> Value
ensureColor color = set (key "hex_color" % _String) color

ensureChange :: Options -> Task -> (Value -> Value) -> Value -> IO ()
ensureChange opts task f task_js = when (new_task /= task_js) $ do
  putTextLn [i|Updating #{task} to #{new_task}|]
  void $ Wreq.postWith opts [i|#{url}/tasks/#{task_id}|] (encode new_task)
 where
  task_id = task.id
  new_task = f task_js

chooseBucket :: Task -> Bucket
chooseBucket t
  | inboxLabel `Set.member` task_labels = inboxBucket
  | isWaiting t = waitingBucket
  | isProject t, not (isMaybe t), not (isAwaiting t), Set.isSubsetOf task_labels nonListLabels = projectBucket
  | t.bucket_id `elem` [inboxBucket, waitingBucket, projectBucket] = backlogBucket
  | otherwise = t.bucket_id
 where
  task_labels = fromMaybe mempty t.labels

chooseColor :: Task -> Text
chooseColor t = case t.priority of
  1 -> wantColor
  2 -> mustColor
  _ -> shouldColor

fetchAll :: Options -> String -> IO (Seq (Value, Task))
fetchAll opts path = go 1 mempty
 where
  go :: Int -> Seq (Value, Task) -> IO (Seq (Value, Task))
  go n xs = do
    newxs <- fetchPage n
    ((xs <> newxs) <>)
      <$> if length newxs == 50
        then fetchPage (n + 1)
        else pure mempty
  fetchPage n = do
    response <- Wreq.getWith (opts & lensVL (param "page") .~ [show n]) path
    either (error . toText) pure $ taskAndJS $ response ^. lensVL responseBody

parentIsNotIdle :: Task -> Bool
parentIsNotIdle t =
  any (\p -> not p.done && projectBucket /= p.bucket_id) . fromMaybe mempty $ t.related_tasks.parenttask

relatedTodo :: Maybe (Set RelatedTask) -> Bool
relatedTodo = not . all (.done) . fromMaybe mempty

isAwaiting, inInbox, isWaiting, isPostponed, isBlocked, isProject, isMaybe, isCategory, isUnsorted :: Task -> Bool
inInbox t = Set.isSubsetOf (fromMaybe mempty t.labels) autoLabels && not (isMaybe t || isAwaiting t || isWaiting t)
isMaybe t = maybeBucket == t.bucket_id
isAwaiting t = awaitingBucket == t.bucket_id
isWaiting t = isBlocked t || parentIsNotIdle t || isPostponed t
isPostponed _ = False -- TODO
isBlocked t = relatedTodo t.related_tasks.blocked
isProject t = relatedTodo t.related_tasks.subtask
isCategory t = Set.member categoryLabel (fromMaybe mempty t.labels)
isUnsorted t = not (isCategory t || relatedTodo t.related_tasks.parenttask)

update :: Options -> IO ()
update opts = do
  tasks <- fetchAll opts [i|#{url}/projects/33/tasks|]
  putTextLn [i|Checking #{length tasks} tasks.|]
  forM_ tasks $ uncurry \value -> runStateT do
    modifyM \t -> ensureLabel opts t parentLabel (isProject t)
    modifyM \t -> ensureLabel opts t inboxLabel (inInbox t)
    modifyM \t -> ensureLabel opts t unsortedLabel (isUnsorted t)
    get >>= \t -> lift $ ensureChange opts t (ensureBucket (chooseBucket t) . ensureColor (chooseColor t)) value

main :: IO ()
main = do
  token <- getToken
  let opts =
        defaults
          & lensVL (header "Authorization") .~ [[i|Bearer #{token}|]]
          & lensVL (header "Content-Type") .~ ["application/json"]
          & lensVL (param "filter_by") .~ ["done"]
          & lensVL (param "filter_value") .~ ["false"]
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

taskAndJS :: LByteString -> Either String (Seq (Value, Task))
taskAndJS s = do
  js <- eitherDecode' s
  tasks <- eitherDecode' s
  pure $ Seq.zip js tasks
