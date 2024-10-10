module StatusScript.Modules.GitHub (runs) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwIO)
import Data.ByteString.Char8 qualified as Bytestring
import Data.Map.Strict qualified as M
import Data.String.Interpolate (i)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, formatTime, getCurrentTime, nominalDay)
import Data.Time.Clock (addUTCTime)
import Data.Time.Format (defaultTimeLocale)
import GitHub
  ( Auth (..)
  , FetchCount (..)
  , WithTotalCount (..)
  , WorkflowRun (..)
  , github
  , mkName
  , optionsWorkflowRunActor
  , optionsWorkflowRunCreated
  , workflowRunsR
  )
import GitHub.Internal.Prelude (Vector)
import Optics
import Reflex
import Reflex.Host.Headless qualified as R
import Relude
import Shh qualified
import StatusScript.Env
import StatusScript.Mode
import StatusScript.Warnings

-- notifications :: R.MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
-- notifications env dMode = do
--   (event, trigger) <- newTriggerEvent
--   liftIO $ env.fork "watchNotifications" (watchNotifications trigger)
--   dNotifications <- holdDyn mempty $ event <&> fmap mkWarning
--   pure $
--     zipDynWith
--       ( \case
--           DND -> const []
--           Normal -> filter (maybe False (Text.isPrefixOf "heilmann") . (.description))
--           Sort -> id
--       )
--       dMode
--       (toList <$> dNotifications)

runs :: R.MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
runs env _ = do
  (eRun, triggerRun) <- newTriggerEvent
  liftIO $ env.fork "watchRuns" (watchRuns triggerRun)
  time <- liftIO getCurrentTime
  holdDyn mempty $ eRun <&> mkRunWarnings time

mkRunWarnings :: UTCTime -> Vector WorkflowRun -> [Warning]
mkRunWarnings time =
  toList
    >>> fmap (\r -> (r.workflowRunHeadBranch, r))
    >>> M.fromListWith (\a b -> if a.workflowRunCreatedAt >= b.workflowRunCreatedAt then a else b)
    >>> toList
    >>> fmap (runToWarning time)

runToWarning :: UTCTime -> WorkflowRun -> Warning
runToWarning time wfRun =
  MkWarning
    { description =
        Just $
          wfRun.workflowRunHeadBranch
            <> " "
            <> (if isNothing subgroup then status <> " " else "")
            <> printDuration
              ( diffUTCTime
                  (if isJust wfRun.workflowRunConclusion then wfRun.workflowRunUpdatedAt else time)
                  wfRun.workflowRunStartedAt
              )
    , group = toEnum 0xeaff -- nf-cod-github_action
    , subgroup
    }
 where
  status = fromMaybe wfRun.workflowRunStatus wfRun.workflowRunConclusion
  subgroup = case status of
    "in_progress" -> Just (toEnum 0xf051f) -- nf-md-timer_send
    "success" -> Just (toEnum 0xf0e1e) -- nf-md-check_bold
    "failure" -> Just (toEnum 0xe654) -- nf-seti-error
    _ -> Nothing

printDuration :: NominalDiffTime -> Text
printDuration diff
  | diff < hour = p "%Mm"
  | diff < day = p "%Hh%Mm"
  | otherwise = p "%dd%Hh%Mm"
 where
  p x = toText $ formatTime defaultTimeLocale x diff

minute :: NominalDiffTime
minute = 60

hour :: NominalDiffTime
hour = 60 * minute

day :: NominalDiffTime
day = 24 * hour

-- parseIssueUrl :: URL -> Maybe (Text, Maybe Char, Text)
-- parseIssueUrl =
--   getUrl >>> Text.stripPrefix "https://api.github.com/repos/" >=> Text.splitOn "/" >>> \case
--     [org, repo, type', id'] -> Just ([i|#{org}/#{repo}|], typeChar type', id')
--     _ -> Nothing
--
-- typeChar :: Text -> Maybe Char
-- typeChar = \case
--   "issues" -> Just $ toEnum 0xeb0c -- nf-cod-issues
--   "pulls" -> Just $ toEnum 0xf04c2 -- nf-md-source_pull
--   _ -> Nothing
--
-- mkWarning :: Notification -> Warning
-- mkWarning n =
--   MkWarning
--     { description = Just description
--     , group = toEnum 0xf02a4 -- nf-md-github
--     , subgroup
--     }
--  where
--   title = n.notificationSubject.subjectTitle
--   (description, subgroup) = case parseIssueUrl =<< n.notificationSubject.subjectURL of
--     Nothing -> (title, Nothing)
--     Just (repo, icon, num) -> ([i|#{repo} \##{num} #{title}|], icon)

getToken :: IO ByteString
getToken =
  Bytestring.strip . toStrict <$> (Shh.exe "rbw" "get" "github.com" "-f" "kass" Shh.|> Shh.captureTrim)

watchRuns :: (Vector WorkflowRun -> IO ()) -> IO ()
watchRuns cb = forever $ do
  token <- getToken
  yesterday <- addUTCTime (-nominalDay) <$> getCurrentTime
  response <-
    github
      (OAuth token)
      ( workflowRunsR
          (mkName Proxy "heilmannsoftware")
          (mkName Proxy "connect")
          (optionsWorkflowRunActor "maralorn" <> optionsWorkflowRunCreated [i|>=#{formatTime defaultTimeLocale "%F" yesterday}|])
          FetchAll
      )
  either throwIO (cb . (.withTotalCountItems)) response
  threadDelay 60_000_000 -- one minute

-- watchNotifications :: (Vector Notification -> IO ()) -> IO ()
-- watchNotifications cb = forever $ do
--   token <- getToken
--   response <- github (OAuth token) (getNotificationsR FetchAll)
--   either (putStrLn . displayException) cb response
--   threadDelay 60_000_000 -- one minute
