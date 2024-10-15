module Vikunja (updateLoop, url, defaultOptions, defaultProject, fetchAll, Task (..)) where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict (modifyM)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode', encode, withText)
import Data.Aeson.Optics
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
  , project_id :: Int
  , priority :: Int
  , bucket_id :: Bucket
  , labels :: Maybe (Set Label)
  , related_tasks :: RelatedTasks
  , kanban_position :: Maybe Double
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
  , bucket_id :: Bucket
  }
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show, Eq, Ord)

data SetLabel = SetLabel
  { created :: Time
  , label_id :: Int
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

chooseBucket :: UTCTime -> Task -> Bucket
chooseBucket now t
  | inboxLabel `Set.member` task_labels = inboxBucket
  | isWaiting now t, not t.done = waitingBucket
  | isProject t, not (t.done || isMaybe t || isAwaiting t), Set.isSubsetOf task_labels nonListLabels = projectBucket
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
fetchAll opts path = do
  r <- go 1
  putTextLn [i|Got #{length r} tasks from #{path}.|]
  pure r
 where
  go :: Int -> IO (Seq (Value, Task))
  go n = do
    newxs <- fetchPage n
    (newxs <>)
      <$> if length newxs == 50
        then go (n + 1)
        else pure mempty
  fetchPage n = do
    response <- Wreq.getWith (opts & lensVL (param "page") .~ [show n]) path
    either (error . toText) pure $ taskAndJS $ response ^. lensVL responseBody

parentIsNotIdle :: Task -> Bool
parentIsNotIdle t =
  any (\p -> not p.done && projectBucket /= p.bucket_id) . fromMaybe mempty $ t.related_tasks.parenttask

relatedTodo :: Maybe (Set RelatedTask) -> Bool
relatedTodo = not . all (.done) . fromMaybe mempty

isAwaiting, isBlocked, isProject, isMaybe, isCategory :: Task -> Bool
isMaybe t = maybeBucket == t.bucket_id
isAwaiting t = awaitingBucket == t.bucket_id
isBlocked t = relatedTodo t.related_tasks.blocked
isProject t = relatedTodo t.related_tasks.subtask || isCategory t
isCategory t = Set.member categoryLabel (fromMaybe mempty t.labels)

inInbox, isWaiting, isUnsorted, isPostponed :: UTCTime -> Task -> Bool
isUnsorted now t = not (inInbox now t || isCategory t || relatedTodo t.related_tasks.parenttask)
isPostponed now t = maybe False ((> now) . zonedTimeToUTC) (t.start_date ^? #_Time % _Just)
inInbox now t =
  Set.isSubsetOf (fromMaybe mempty t.labels) autoLabels
    && not (isMaybe t || isAwaiting t || isWaiting now t || t.done)
isWaiting now t = isBlocked t || parentIsNotIdle t || isPostponed now t

defaultProject :: Int
defaultProject = 33

updateDefaultProject :: Options -> IO ()
updateDefaultProject opts = do
  tasks <- fetchAll opts [i|#{url}/projects/#{defaultProject}/tasks|]
  now <- getCurrentTime
  forM_ tasks $ uncurry \value -> runStateT do
    modifyM \t -> ensureLabel opts t parentLabel (isProject t)
    modifyM \t -> ensureLabel opts t inboxLabel (inInbox now t)
    modifyM \t -> ensureLabel opts t unsortedLabel (isUnsorted now t)
    get >>= \t ->
      lift $
        ensureChange
          opts
          t
          ( ensureBucket (chooseBucket now t)
              >>> ensureColor (chooseColor t)
              >>> ensureRecentRepetition now t
          )
          value

update :: Options -> IO ()
update opts = do
  updateCheckLists opts
  updateDefaultProject opts

updateCheckLists :: Options -> IO ()
updateCheckLists opts = do
  tasks <- fetchAll opts [i|#{url}/projects/40/tasks|]
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

taskAndJS :: LByteString -> Either String (Seq (Value, Task))
taskAndJS s = do
  js <- eitherDecode' s
  tasks <- eitherDecode' s
  pure $ Seq.zip js tasks
