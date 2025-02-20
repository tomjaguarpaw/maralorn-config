module StatusScript.App (publishSockets, notify) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Exception.Safe (catchAny)
import Data.String.Interpolate (i)
import Data.Time.Clock.POSIX qualified as Time
import Optics
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import Reflex.Host.Headless qualified as R
import Relude
import Say (sayErr)
import Shh (ExecReference (Absolute), load)
import StatusScript.CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.Mode
import StatusScript.Mode qualified as Mode
import StatusScript.Modules.Audio qualified as Audio
import StatusScript.Modules.Calendar qualified as Calendar
import StatusScript.Modules.GitHub qualified as GitHub
import StatusScript.Modules.Hyprland (hyprlandWorkspaces)
import StatusScript.Modules.IdleState qualified as IdleState
import StatusScript.Modules.Klog qualified as Klog
import StatusScript.Modules.Mail qualified as Mail
import StatusScript.Modules.Mako
import StatusScript.Modules.Network qualified as Network
import StatusScript.Modules.Ping
import StatusScript.Modules.Player qualified as Player
import StatusScript.Modules.SoftwareFeed qualified as SoftwareFeed
import StatusScript.Modules.Timer qualified as Timer
import StatusScript.Modules.Vikunja
import StatusScript.Notify (notifyHomeAssistant)
import StatusScript.PublishSocket
import StatusScript.Warnings
import System.Environment qualified as Env
import System.FSNotify qualified as Notify

Shh.load Shh.Absolute ["mkdir"]

missingExecutables :: IO [FilePath]
runWithEnv :: (Env -> forall m t. MonadHeadlessApp t m => m ()) -> IO ()
runWithEnv w = Notify.withManager \watch_manager -> do
  homeDir <- Env.getEnv "HOME"
  job_queue <- STM.newTQueueIO
  let env =
        MkEnv
          { homeDir
          , fork = curry (STM.writeTQueue job_queue >>> atomically)
          , watch_manager
          }
  R.runHeadlessApp $ do
    w env
    (end_event, trigger) <- newTriggerEvent
    let run_job_queue = do
          (job_name, job) <- atomically $ STM.readTQueue job_queue
          Async.concurrently_ run_job_queue $
            catchAny job \e -> do
              sayErr [i|In async job "#{job_name}" error: #{e}|]
              Exception.throwIO e
    void $ liftIO $ Async.async $ catchAny run_job_queue \_ -> trigger ()
    pure end_event
  sayErr "Exiting because of previous errors."
  exitFailure

warnings :: MonadHeadlessApp t m => Env -> m (Dynamic t [Warning])
warnings env = do
  mode <- Mode.getMode env
  ping_dyn <- ping' env
  software_feed_event <- SoftwareFeed.softwareFeed env mode
  mail_events <- Mail.mail env mode
  notification_dyn <- notifications env mode
  klog_dyn <- Klog.warnings env mode
  let mode_warning =
        mode
          <&> ( modeIcon >>> maybe [] \m ->
                  [ MkWarning
                      { group = m
                      , description = []
                      , subgroup = Nothing
                      , barDisplay = Count
                      , heading = "Mode"
                      }
                  ]
              )
  gh_runs_dyn <- GitHub.runs env mode
  task_dyn <- tasks env mode
  pure $
    concat
      <$> sequence
        [ mode_warning
        , ping_dyn
        , notification_dyn
        , mail_events
        , software_feed_event
        , gh_runs_dyn
        , task_dyn
        , klog_dyn
        ]

prepareSocketsDir :: MonadIO m => m ()
prepareSocketsDir = do
  reportMissing missingExecutables
  liftIO $ mkdir "-p" socketsDir

uptime :: (Reflex t, MonadIO m) => m (Dynamic t Int)
uptime = liftIO $ constDyn . round <$> Time.getPOSIXTime

notify :: IO ()
notify = runWithEnv \env -> do
  notifyHomeAssistant env =<< warnings env

publishSockets :: IO ()
publishSockets = runWithEnv \env -> do
  prepareSocketsDir
  publishJson' env "uptime" =<< uptime
  warnings_dyn <- warnings env
  publishJson' env "warnings" (warningSections <$> warnings_dyn)
  publishJson' env "warninggroups" (warningGroups <$> warnings_dyn)
  publishJson env "players" =<< Player.playerModule env
  publishJson env "calendar" =<< Calendar.calendar env
  publishJson' env "idle_state" =<< IdleState.idleState env
  publishJson env "timers" =<< Timer.timers env
  publishJson env "networks" =<< Network.networkState env
  publishJson env "audio" =<< Audio.audioInfos =<< Audio.audioUpdateEvent env
  publishJson' env "workspaces" =<< hyprlandWorkspaces env
