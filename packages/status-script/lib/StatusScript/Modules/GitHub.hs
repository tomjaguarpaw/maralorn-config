module StatusScript.Modules.GitHub (runs) where

import Control.Exception.Safe (throwIO)
import Data.ByteString.Char8 qualified as Bytestring
import Data.Map.Strict qualified as M
import Data.String.Interpolate (i)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, formatTime, getCurrentTime, nominalDay)
import Data.Time.Clock (addUTCTime)
import Data.Time.Format (defaultTimeLocale)
import GitHub
  ( Auth (..)
  , Branch (..)
  , Error
  , FetchCount (..)
  , WithTotalCount (..)
  , WorkflowRun (..)
  , branchesForR
  , github
  , mkName
  , optionsWorkflowRunActor
  , optionsWorkflowRunCreated
  , workflowRunsR
  )
import Reflex
import Reflex.Host.Headless qualified as R
import Relude
import StatusScript.CommandUtil
import StatusScript.Env
import StatusScript.Mode
import StatusScript.ReflexUtil
import StatusScript.Warnings

data IsActive = IsActive

repos :: [(Text, Text)]
repos = [("heilmannsoftware", "connect")]

user :: Text
user = "maralorn"

runs :: R.MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
runs env _ = do
  token <- liftIO getToken
  ev <- flexiblePoll env (fetchRuns token <&> second (maybe 300_000_000 (const 60_000_000)))
  holdDyn mempty ev

mkRunWarnings :: UTCTime -> [WorkflowRun] -> ([Warning], Maybe IsActive)
mkRunWarnings time =
  toList
    >>> fmap (\r -> (r.workflowRunHeadBranch, r))
    >>> M.fromListWith (\a b -> if a.workflowRunCreatedAt >= b.workflowRunCreatedAt then a else b)
    >>> toList
    >>> fmap (runToWarning time)
    >>> unzip
    >>> second (getFirst . foldMap First)

runToWarning :: UTCTime -> WorkflowRun -> (Warning, Maybe IsActive)
runToWarning time wfRun =
  ( MkWarning
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
  , case status of
      "in_progress" -> Just IsActive
      "queued" -> Just IsActive
      "pending" -> Just IsActive
      _ -> Nothing
  )
 where
  status = fromMaybe wfRun.workflowRunStatus wfRun.workflowRunConclusion
  subgroup = case status of
    "in_progress" -> Just (toEnum 0xf051f) -- nf-md-timer_send
    "success" -> Just (toEnum 0xf0e1e) -- nf-md-check_bold
    "failure" -> Just (toEnum 0xe654) -- nf-seti-error
    "queued" -> Just (toEnum 0xf1571) -- nf-md-human_queue
    "pending" -> Just (toEnum 0xf051f) -- nf-md-timer_sand
    "cancelled" -> Just (toEnum 0xf073a) -- nf-md-cancel
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

getToken :: IO Auth
getToken = OAuth . Bytestring.strip <$> readFileBS "/run/agenix/github-read-workflow-token"

fetchRuns :: Auth -> IO ([Warning], Maybe IsActive)
fetchRuns token = do
  yesterday <- addUTCTime (-nominalDay) <$> getCurrentTime
  response <- forM repos \(owner, repo) -> retryTimeout 10 30 $ do
    response <-
      fmap (toList . (.withTotalCountItems)) $
        unpackError
          =<< github
            token
            ( workflowRunsR
                (mkName Proxy owner)
                (mkName Proxy repo)
                ( optionsWorkflowRunActor user
                    <> optionsWorkflowRunCreated [i|>=#{formatTime defaultTimeLocale "%F" yesterday}|]
                )
                FetchAll
            )
    if null response
      then pure response
      else do
        branches <-
          fmap (fmap (.branchName) . toList) $
            unpackError
              =<< github
                token
                ( branchesForR
                    (mkName Proxy owner)
                    (mkName Proxy repo)
                    FetchAll
                )
        pure $ filter (\r -> r.workflowRunHeadBranch `elem` branches) response
  time <- liftIO getCurrentTime
  pure $ mkRunWarnings time . fold . catMaybes $ response

unpackError :: Either Error a -> IO a
unpackError = either throwIO pure
