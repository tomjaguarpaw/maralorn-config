module Maralorn.Taskwarrior (getInbox) where

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time qualified as Time
import Data.UUID qualified as UUID
import Maralorn.Prelude
import Taskwarrior.IO qualified as Task
import Taskwarrior.Status qualified as Status
import Taskwarrior.Task qualified as Task

projectTags :: Set Task.Tag
projectTags = fromList ["root", "kategorie"]

getInbox :: IO [Task.Task]
getInbox = do
  tasks <- allRelevantTasks
  now <- Time.getCurrentTime
  let active_tasks = tasks & HashMap.keysSet
      tagged_tasks =
        tasks
          & toList
          % filter (\task -> not (Set.isSubsetOf task.tags projectTags))
          %> (.uuid)
          % HashSet.fromList
      blocked_tasks :: HashSet UUID.UUID =
        tasks
          & toList
          % filter (\task -> any (`HashSet.member` active_tasks) task.depends)
          %> (.uuid)
          % fromList
      children :: HashMap UUID.UUID (HashSet UUID.UUID) =
        tasks
          & toList
            % mapMaybe (\task -> getParent task <&> (,HashSet.singleton task.uuid))
            % HashMap.fromListWith (<>)
      waiting_tasks :: HashSet UUID.UUID =
        tasks
          & toList
          % filter
            ( \task -> case task.wait of
                Just wait_until -> wait_until > now
                Nothing -> False
            )
          %> (.uuid)
          % fromList
      inhibitedTasks :: HashSet UUID.UUID = foldMap (closure children) (blocked_tasks <> waiting_tasks <> tagged_tasks)
  pure $
    toList tasks
      & filter \task -> not (HashMap.member task.uuid children) && not (HashSet.member task.uuid inhibitedTasks)

closure :: Hashable a => HashMap a (HashSet a) -> a -> HashSet a
closure mapping = go
 where
  go = \x ->
    HashMap.lookup x mapping
      & maybe mempty (foldMap go)
        % HashSet.insert x

getParent :: Task.Task -> Maybe UUID.UUID
getParent = \task ->
  Map.lookup "partof" task.uda
    >>= Aeson.fromJSON % \case
      Aeson.Success uuid -> Just uuid
      Aeson.Error err -> error ("invalid partof uda " <> toText err)

allRelevantTasks :: IO (HashMap UUID.UUID Task.Task)
allRelevantTasks = do
  Task.getTasks ["-COMPLETED", "-DELETED"]
    <&> filter (\task -> task.status == Status.Pending)
    %> (\task -> (task.uuid, task))
    % HashMap.fromList
