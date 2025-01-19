module Vikunja (updateLoop, url, defaultOptions, defaultProject, fetchAll, Task (..), defaultKanban, Query (..)) where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict (modifyM)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, fromJSON, withText)
import Data.Aeson.Optics
import Data.Map.Optics (toMapOf)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time
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

maybeBucket, awaitingBucket, inboxBucket, waitingBucket, projectBucket, backlogBucket :: Bucket
maybeBucket = 52
awaitingBucket = 53
inboxBucket = 55
waitingBucket = 56
projectBucket = 58
backlogBucket = 42

scheduleBuckets :: Set Bucket
scheduleBuckets =
  fromList
    [ 50 -- Doing,
    , 49 -- Heute,
    , 48 -- Diese Woche,
    , 42 -- Backlog
    ]

wantColor, mustColor, shouldColor, urgentColor :: Text
wantColor = "1a5fb4"
mustColor = "ffbe6f"
shouldColor = ""
urgentColor = "ff0000"

getToken :: IO Text
getToken = Text.strip . decodeUtf8 <$> readFileBS "/run/agenix/vikunja-token"

newtype Label = Label
  { id :: Int
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

newtype Bucket = Bucket Int
  deriving newtype (Num, FromJSON, ToJSON, Show, Eq, Ord)

newtype View = View Int
  deriving newtype (Num, FromJSON, ToJSON, Show, Eq, Ord)

newtype Project = Project Int
  deriving newtype (Num, FromJSON, ToJSON, Show, Eq, Ord)

newtype Time = Time (Maybe ZonedTime)
  deriving newtype (Show)
  deriving stock (Generic)

instance Eq Time where
  Time (Just (ZonedTime a b)) == Time (Just (ZonedTime c d)) = a == c && b == d
  Time Nothing == Time Nothing = True
  _ == _ = False

timeFormatString :: String
timeFormatString = "%FT%T%Ez"

defaultTime :: String
defaultTime = "0001-01-01T00:00:00Z"

instance FromJSON Time where
  parseJSON =
    withText "Datetime" $
      fmap Time . \case
        "0001-01-01T00:00:00Z" -> pure Nothing
        x -> fmap Just . parseTimeM True defaultTimeLocale timeFormatString . toString $ x

instance ToJSON Time where
  toJSON =
    String . toText . \case
      Time Nothing -> defaultTime
      Time (Just x) -> formatTime defaultTimeLocale timeFormatString x

data Task = Task
  { id :: Int
  , done :: Bool
  , title :: Text
  , description :: Text
  , created :: Time
  , start_date :: Time
  , repeat_after :: Int
  , repeat_mode :: Int
  , project_id :: Project
  , priority :: Int
  , bucket_id :: Bucket
  , labels :: Maybe (Set Label)
  , related_tasks :: RelatedTasks
  , position :: Double
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq)

data RelatedTasks = RelatedTasks
  { subtask :: Maybe (Set RelatedTask)
  , blocked :: Maybe (Set RelatedTask)
  , parenttask :: Maybe (Set RelatedTask)
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data RelatedTask = RelatedTask
  { done :: Bool
  , id :: Int
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data SetLabel = SetLabel
  { created :: Time
  , label_id :: Int
  }
  deriving anyclass (ToJSON)
  deriving stock (Generic, Show)

newtype SetBucket = SetBucket
  { task_id :: Int
  }
  deriving anyclass (ToJSON)
  deriving stock (Generic, Show)

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

ensureBucket :: Options -> Task -> Project -> View -> Bucket -> IO Task
ensureBucket opts task project view' bucket = do
  when (new_task /= task) $ do
    putTextLn [i|Setting bucket #{bucket} for #{task}|]
    void $
      Wreq.postWith opts [i|#{url}/projects/#{project}/views/#{view'}/buckets/#{bucket}/tasks|] (encode $ SetBucket task.id)
  pure new_task
 where
  new_task = task & #bucket_id .~ bucket

ensureUncheckedDescription :: UTCTime -> Task -> Value -> Value
ensureUncheckedDescription now t
  | isPostponed now t =
      over
        (key "description" % _String)
        ( Text.replace "data-checked=\"true\"" "data-checked=\"false\""
            >>> Text.replace "checked=\"checked\"" ""
        )
  | otherwise = id

ensureRecentRepetition :: UTCTime -> Task -> Value -> Value
ensureRecentRepetition now t
  | t.repeat_mode == 0
  , t.repeat_after /= 0
  , maybe
      False
      ((<= now) . addUTCTime oneDay . zonedTimeToUTC)
      (t.start_date ^? #_Time % _Just) =
      over (key "start_date" % _JSON % timeLocaltime) (addLocalTime oneDay)
  | otherwise = id

oneDay :: NominalDiffTime
oneDay = 86400

timeLocaltime :: AffineTraversal' Time LocalTime
timeLocaltime = #_Time % _Just % lens zonedTimeToLocalTime (\z l -> z{zonedTimeToLocalTime = l})

ensureColor :: Text -> Value -> Value
ensureColor color = set (key "hex_color" % _String) color

ensureChange :: Options -> Task -> (Value -> Value) -> Value -> IO ()
ensureChange opts task f task_js = when (new_task /= task_js) $ do
  putTextLn [i|Updating #{task} to #{new_task}|]
  void $ Wreq.postWith opts [i|#{url}/tasks/#{task_id}|] (encode new_task)
 where
  task_id = task.id
  new_task = f task_js

chooseBucket :: UTCTime -> Map Int Task -> Task -> Bucket
chooseBucket now ts t
  | inboxLabel `Set.member` task_labels = inboxBucket
  | isWaiting now ts t, not t.done = waitingBucket
  | isProject t, not (t.done || isMaybe t || isAwaiting t || isScheduled t) = projectBucket
  | t.bucket_id `elem` [inboxBucket, waitingBucket, projectBucket] = backlogBucket
  | otherwise = t.bucket_id
 where
  task_labels = fromMaybe mempty t.labels

chooseColor :: Task -> Text
chooseColor t = case t.priority of
  1 -> wantColor
  2 -> mustColor
  3 -> urgentColor
  _ -> shouldColor

data Query = MkQuery
  { opts :: Options
  , path :: String
  , inBuckets :: Bool
  }

fetchAll :: Query -> IO (Seq (Value, Task))
fetchAll query@MkQuery{path} = do
  r <- go 1
  putTextLn [i|Got #{length r} tasks from #{path}.|]
  pure r
 where
  go :: Int -> IO (Seq (Value, Task))
  go n = do
    newxs <- fetchPage n
    (newxs <>)
      <$> if length newxs >= 50
        then go (n + 1)
        else pure mempty
  fetchPage n = do
    response <- Wreq.getWith (query.opts & lensVL (param "page") .~ [show n]) path
    pure $ Seq.fromList $ (if query.inBuckets then taskAndJSBucket else taskAndJS) $ response ^. lensVL responseBody

parentIsNotIdle :: Map Int Task -> Task -> Bool
parentIsNotIdle ts t =
  any (\p -> not p.done && notElemOf (ix p.id % #bucket_id) projectBucket ts) . fromMaybe mempty $
    t.related_tasks.parenttask

relatedTodo :: Maybe (Set RelatedTask) -> Bool
relatedTodo = not . all (.done) . fromMaybe mempty

isAwaiting, isBlocked, isProject, isMaybe, isCategory, isScheduled :: Task -> Bool
isMaybe t = maybeBucket == t.bucket_id
isAwaiting t = awaitingBucket == t.bucket_id
isBlocked t = relatedTodo t.related_tasks.blocked
isProject t = relatedTodo t.related_tasks.subtask || isCategory t
isCategory t = Set.member categoryLabel (fromMaybe mempty t.labels)
isScheduled t = t.bucket_id `Set.member` scheduleBuckets

isPostponed :: UTCTime -> Task -> Bool
isPostponed now t = maybe False ((> now) . zonedTimeToUTC) (t.start_date ^? #_Time % _Just)

isUnsorted, inInbox, isWaiting :: UTCTime -> Map Int Task -> Task -> Bool
isWaiting now ts t = isBlocked t || parentIsNotIdle ts t || isPostponed now t
isUnsorted now ts t =
  diffUTCTime now (t ^. #created % coerced % to (fmap zonedTimeToUTC) % non now)
    > (2 * 24 * 60 * 60)
    && not
      ( t.done
          || inInbox now ts t
          || isCategory t
          || relatedTodo t.related_tasks.parenttask
      )
inInbox now ts t =
  not (isProject t || isCategory t || isScheduled t || isMaybe t || isAwaiting t || isWaiting now ts t || t.done)

defaultProject :: Project
defaultProject = 33

defaultKanban :: View
defaultKanban = 32

updateDefaultProject :: Options -> IO ()
updateDefaultProject opts = do
  tasks <-
    fetchAll MkQuery{opts, path = [i|#{url}/projects/#{defaultProject}/views/#{defaultKanban}/tasks|], inBuckets = True}
  let taskMap = toMapOf (folded % _2 % reindexed (.id) selfIndex) tasks
  now <- getCurrentTime
  forM_ tasks $ uncurry \value -> runStateT do
    modifyM \t -> ensureLabel opts t parentLabel (isProject t)
    modifyM \t -> ensureLabel opts t inboxLabel (inInbox now taskMap t)
    modifyM \t -> ensureLabel opts t unsortedLabel (isUnsorted now taskMap t)
    modifyM \t -> ensureBucket opts t defaultProject defaultKanban (chooseBucket now taskMap t)
    get >>= \t ->
      lift $
        ensureChange
          opts
          t
          (ensureColor (chooseColor t) >>> ensureRecentRepetition now t)
          value

update :: Options -> IO ()
update opts = do
  updateCheckLists opts
  updateDefaultProject opts

updateCheckLists :: Options -> IO ()
updateCheckLists opts = do
  tasks <- fetchAll MkQuery{opts, path = [i|#{url}/projects/40/tasks|], inBuckets = False}
  now <- getCurrentTime
  forM_ tasks $ \(value, t) ->
    ensureChange opts t (ensureRecentRepetition now t >>> ensureUncheckedDescription now t) value

defaultOptions :: IO Options
defaultOptions = do
  token <- getToken
  pure $
    defaults
      & lensVL (header "Authorization") .~ [[i|Bearer #{token}|]]
      & lensVL (header "Content-Type") .~ ["application/json"]

updateLoop :: IO ()
updateLoop = do
  opts <-
    defaultOptions
      <&> (lensVL (param "filter_by") .~ ["done"])
        . (lensVL (param "filter_value") .~ ["false"])
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

taskAndJS :: LByteString -> [(Value, Task)]
taskAndJS = itoListOf (_Array % folded % selfIndex % to fromJSON % folded)

taskAndJSBucket :: LByteString -> [(Value, Task)]
taskAndJSBucket = itoListOf (_Array % folded % key "tasks" % _Array % folded % selfIndex % to fromJSON % folded)
