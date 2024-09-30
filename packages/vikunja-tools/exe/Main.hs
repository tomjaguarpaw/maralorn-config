module Main where

import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Network.Wreq
import Network.Wreq qualified as Wreq
import Optics
import Relude

url :: Text
url = "https://todo.darmstadt.ccc.de/api/v1"

inboxLabel :: Label
inboxLabel = Label 11

parentLabel :: Label
parentLabel = Label 12

blockedLabel :: Label
blockedLabel = Label 13

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
    print (if should_exist then "setting" else "unsetting" :: Text, label, "for" :: Text, task)
    void $
      if should_exist
        then Wreq.putWith opts [i|#{url}/tasks/#{task_id}/labels|] (encode $ SetLabel task.created label.id)
        else Wreq.deleteWith opts [i|#{url}/tasks/#{task_id}/labels/#{label_id}|]

  pure new_task
 where
  task_id = task.id
  label_id = label.id
  new_task = task & #labels % non mempty %~ (if should_exist then Set.insert else Set.delete) label

main :: IO ()
main = do
  token <- getToken
  let opts =
        defaults
          & lensVL (header "Authorization") .~ [[i|Bearer #{token}|]]
          & lensVL (header "Content-Type") .~ ["application/json"]
  response <- Wreq.getWith opts [i|#{url}/tasks/all|]
  tasks :: [Task] <- either (error . toText) pure $ eitherDecode' $ response ^. lensVL responseBody
  forM_ tasks \t1 -> do
    t2 <- ensureLabel opts t1 blockedLabel (not (all (.done) (fromMaybe mempty t1.related_tasks.blocked)))
    t3 <- ensureLabel opts t2 parentLabel (not (all (.done) (fromMaybe mempty t2.related_tasks.subtask)))
    ensureLabel opts t3 inboxLabel (not (any (/= inboxLabel) (fromMaybe mempty t3.labels)))
