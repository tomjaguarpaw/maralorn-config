module Main (main) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Exception.Safe (catchAny)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time.Clock.POSIX qualified as Time
import Maralorn.Prelude
import Reflex
import Reflex.Host.Headless qualified as R
import Shh (ExecReference (Absolute), load)
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.Mode
import StatusScript.Mode qualified as Mode
import StatusScript.Modules.Audio qualified as Audio
import StatusScript.Modules.Calendar qualified as Calendar
import StatusScript.Modules.GitHub qualified as GitHub
import StatusScript.Modules.Hyprland (hyprlandWorkspaces)
import StatusScript.Modules.IdleState qualified as IdleState
import StatusScript.Modules.Mail qualified as Mail
import StatusScript.Modules.Mako
import StatusScript.Modules.Network qualified as Network
import StatusScript.Modules.Ping
import StatusScript.Modules.Player qualified as Player
import StatusScript.Modules.SoftwareFeed qualified as SoftwareFeed
import StatusScript.Modules.Timer qualified as Timer
import StatusScript.PublishSocket qualified as PublishSocket
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings
import System.Environment qualified as Env
import System.FSNotify qualified as Notify

Shh.load Shh.Absolute ["mkdir"]

missingExecutables :: IO [FilePath]

data WarningGroup = MkWarningGroup
  { name :: Char
  , count :: Int
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)

main :: IO ()
main = Notify.withManager \watch_manager -> do
  homeDir <- Env.getEnv "HOME"
  job_queue <- STM.newTQueueIO
  let env =
        MkEnv
          { homeDir
          , fork = curry (STM.writeTQueue job_queue % atomically)
          , watch_manager
          }
  CommandUtil.reportMissing missingExecutables
  mkdir "-p" PublishSocket.socketsDir
  now' :: Int <- Time.getPOSIXTime <&> round
  R.runHeadlessApp do
    start <- getPostBuild
    PublishSocket.publishJson env "uptime" (start $> now')
    mode <- Mode.getMode env
    ping_dyn <- ping' env
    software_feed_event <- SoftwareFeed.softwareFeed env mode
    mail_events <- Mail.mail env mode
    notification_dyn <- notifications env mode
    let mode_warning =
          mode
            <&> ( modeIcon >>> maybe [] \m ->
                    [ MkWarning
                        { group = m
                        , description = Nothing
                        , subgroup = Nothing
                        }
                    ]
                )
    gh_runs_dyn <- GitHub.runs env mode
    let warnings =
          concat
            <$> sequence
              [ ping_dyn
              , software_feed_event
              , mail_events
              , notification_dyn
              , mode_warning
              ]
    PublishSocket.publishJson' env "warnings" (concat <$> sequence [warnings, gh_runs_dyn])
    PublishSocket.publishJson'
      env
      "warninggroups"
      ( warnings
          <&> fmap (.group)
          % NonEmpty.group
          %> ( \group' ->
                MkWarningGroup
                  { name = head group'
                  , count = length group'
                  }
             )
      )
    player_events <- Player.playerModule env
    PublishSocket.publishJson env "players" player_events
    appointments_event <- Calendar.calendar env
    PublishSocket.publishJson env "calendar" appointments_event
    idle_events <- IdleState.idleState env >>= ReflexUtil.updatedAndStart
    PublishSocket.publishJson env "idle_state" idle_events
    timer_events <- Timer.timers env
    PublishSocket.publishJson env "timers" timer_events
    network_events <- Network.networkState env
    PublishSocket.publishJson env "networks" network_events
    audio_event <- Audio.audioUpdateEvent env
    audio_info_event <- Audio.audioInfos audio_event
    PublishSocket.publishJson env "audio" audio_info_event
    hyprland_workspaces <- hyprlandWorkspaces env
    PublishSocket.publishJson' env "workspaces" hyprland_workspaces
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
