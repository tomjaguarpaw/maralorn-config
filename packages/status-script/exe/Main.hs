module Main (main) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NonEmpty
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh (ExecReference (Absolute), load)
import System.FSNotify qualified as Notify

import StatusScript.CommandUtil qualified as ReflexUtil
import StatusScript.Mode qualified as Mode
import StatusScript.Modules.Audio qualified as Audio
import StatusScript.Modules.BootState qualified as BootState
import StatusScript.Modules.Calendar qualified as Calendar
import StatusScript.Modules.ConfigPull qualified as ConfigPull
import StatusScript.Modules.ConfigStale qualified as ConfigStale
import StatusScript.Modules.Git qualified as Git
import StatusScript.Modules.Mail qualified as Mail
import StatusScript.Modules.Ping qualified as Ping
import StatusScript.Modules.Player qualified as Player
import StatusScript.Modules.SoftwareFeed qualified as SoftwareFeed
import StatusScript.Modules.Tasks qualified as Tasks
import StatusScript.Modules.Timer qualified as Timer
import StatusScript.PublishSocket qualified as PublishSocket
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings qualified as Warnings

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
  ReflexUtil.reportMissing missingExecutables
  mkdir "-p" PublishSocket.socketsDir
  R.runHeadlessApp do
    mode <- Mode.getMode watch_manager
    ping_event <- Ping.ping
    software_feed_event <- SoftwareFeed.softwareFeed watch_manager mode
    boot_state_event <- BootState.bootState watch_manager mode
    config_pull_event <- ConfigPull.pullNeeded mode
    (git_warnings, dirties) <- Git.gitEvents watch_manager mode
    config_stale_event <- ConfigStale.configStale mode dirties
    mail_events <- Mail.mail watch_manager mode
    inbox_events <- Tasks.tasks watch_manager mode
    warnings <- ReflexUtil.concatEvents [ping_event, software_feed_event, boot_state_event, config_pull_event, git_warnings, config_stale_event, mail_events, inbox_events]
    PublishSocket.publishJson "warnings" warnings
    PublishSocket.publishJson
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
    start <- R.getPostBuild
    PublishSocket.publish "mode" (ReflexUtil.taggedAndUpdated mode start <&> show)
    player_events <- Player.playerModule
    appointments_event <- Calendar.calendar
    PublishSocket.publishJson "calendar" appointments_event
    PublishSocket.publishJson "players" player_events
    timer_events <- Timer.timers watch_manager
    PublishSocket.publishJson "timers" timer_events
    audio_event <- Audio.audioUpdateEvent
    audio_info_event <- Audio.audioInfos audio_event
    PublishSocket.publishJson "audio" audio_info_event
    pure R.never -- We have no exit condition.
