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
import Reflex
import Reflex.Host.Headless qualified as R
import Relude
import StatusScript.Env
import StatusScript.Mode
import StatusScript.Warnings

runs :: R.MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
runs env _ = do
  (eRun, triggerRun) <- newTriggerEvent
  liftIO $ env.fork "watchRuns" (watchRuns triggerRun)
  holdDyn mempty eRun

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
        [ wfRun.workflowRunHeadBranch
            <> " "
            <> (if isNothing subgroup then status <> " " else "")
            <> printDuration
              ( diffUTCTime
                  (if isJust wfRun.workflowRunConclusion then wfRun.workflowRunUpdatedAt else time)
                  wfRun.workflowRunStartedAt
              )
        ]
    , heading = "GitHub Actions"
    , barDisplay = None
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

minute, hour, day :: NominalDiffTime
minute = 60
hour = 60 * minute
day = 24 * hour

getToken :: IO ByteString
getToken = Bytestring.strip <$> readFileBS "/run/agenix/github-read-workflow-token"

watchRuns :: ([Warning] -> IO ()) -> IO ()
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
  time <- liftIO getCurrentTime
  either throwIO (cb . mkRunWarnings time . (.withTotalCountItems)) response
  threadDelay 60_000_000 -- one minute
