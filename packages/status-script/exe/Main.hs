module Main (main) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NonEmpty
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh (ExecReference (Absolute), load)
import System.FSNotify qualified as Notify

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.Time.Clock.POSIX qualified as Time
import StatusScript.CommandUtil qualified as ReflexUtil
import StatusScript.Env (Env (..))
import StatusScript.Mode qualified as Mode
import StatusScript.Modules.Audio qualified as Audio
import StatusScript.Modules.BootState qualified as BootState
import StatusScript.Modules.Calendar qualified as Calendar
import StatusScript.Modules.ConfigPull qualified as ConfigPull
import StatusScript.Modules.ConfigStale qualified as ConfigStale
import StatusScript.Modules.Git qualified as Git
import StatusScript.Modules.Mail qualified as Mail
import StatusScript.Modules.Mako qualified as Mako
import StatusScript.Modules.Ping qualified as Ping
import StatusScript.Modules.Player qualified as Player
import StatusScript.Modules.SoftwareFeed qualified as SoftwareFeed
import StatusScript.Modules.Tasks qualified as Tasks
import StatusScript.Modules.Timer qualified as Timer
import StatusScript.PublishSocket qualified as PublishSocket
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings qualified as Warnings
import System.Environment qualified as Env

Shh.load Shh.Absolute ["mkdir"]
missingExecutables :: IO [FilePath]

data WarningGroup = MkWarningGroup
  { name :: Text
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
  ReflexUtil.reportMissing missingExecutables
  mkdir "-p" PublishSocket.socketsDir
  now :: Int <- Time.getPOSIXTime <&> round
  R.runHeadlessApp do
    start <- R.getPostBuild
    PublishSocket.publishJson env "uptime" (start $> now)
    mode <- Mode.getMode env
    ping_event <- Ping.ping env
    software_feed_event <- SoftwareFeed.softwareFeed env mode
    boot_state_event <- BootState.bootState env mode
    config_pull_event <- ConfigPull.pullNeeded env mode
    (git_warnings, dirties) <- Git.gitEvents env mode
    config_stale_event <- ConfigStale.configStale env mode dirties
    mail_events <- Mail.mail env mode
    inbox_events <- Tasks.tasks env mode
    notification_events <- Mako.notifications env
    warnings <-
      ReflexUtil.concatEvents
        [ ping_event
        , boot_state_event
        , config_pull_event
        , git_warnings
        , software_feed_event
        , config_stale_event
        , mail_events
        , inbox_events
        , notification_events
        , start $> []
        ]
    PublishSocket.publishJson env "warnings" warnings
    PublishSocket.publishJson
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
    PublishSocket.publish env "mode" (ReflexUtil.taggedAndUpdated mode start <&> show)
    player_events <- Player.playerModule env
    appointments_event <- Calendar.calendar env
    PublishSocket.publishJson env "calendar" appointments_event
    PublishSocket.publishJson env "players" player_events
    timer_events <- Timer.timers env
    PublishSocket.publishJson env "timers" timer_events
    audio_event <- Audio.audioUpdateEvent env
    audio_info_event <- Audio.audioInfos audio_event
    PublishSocket.publishJson env "audio" audio_info_event
    (end_event, trigger) <- R.newTriggerEvent
    let run_job_queue = do
          (job_name, job) <- atomically $ STM.readTQueue job_queue
          Async.concurrently_
            run_job_queue
            ( Exception.catchJust
                (\e -> if isJust (fromException @Async.AsyncCancelled e) then Nothing else Just e)
                job
                \e -> do
                  sayErr [i|In async job "#{job_name}" error: #{e}|]
                  Exception.throwIO e
            )
    void $ liftIO $ Async.async $ Exception.catch run_job_queue \(_ :: SomeException) -> trigger ()
    pure end_event -- We have no exit condition.
  sayErr "Exiting because of previous errors."
  exitFailure
