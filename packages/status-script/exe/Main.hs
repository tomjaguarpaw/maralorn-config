module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch)
import Control.Exception qualified as Exception

import Data.ByteString.Char8 qualified as ByteStringChar
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC

import Data.Text qualified as Text
import Maralorn.Prelude
import Maralorn.Taskwarrior qualified

import Taskwarrior.Task qualified

import Shh (ExecReference (Absolute), Proc, captureTrim, exe, ignoreFailure, load, (&>), (|>))
import Shh qualified

import Control.Concurrent qualified as Conc
import Data.Set qualified as Set
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Reflex.Network qualified as R
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FSNotify qualified as Notify
import System.FilePath ((</>))

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NonEmpty

import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Modules.Audio qualified as Audio
import StatusScript.Modules.Bluetooth qualified as Bluetooth
import StatusScript.Modules.Player qualified as Player
import StatusScript.Modules.Timer qualified as Timer
import StatusScript.PublishSocket qualified as PublishSocket
import StatusScript.ReflexUtil (oneSecond)
import StatusScript.ReflexUtil qualified as ReflexUtil

data Mode = Klausur | Orga | Code | Gaming | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

Shh.load Shh.Absolute ["git", "khal", "notmuch", "readlink", "nix", "nix-diff", "jq", "mkdir"]

missingExecutables :: IO [FilePath]
modes :: [Mode]
modes = enumFrom Klausur

getMode :: R.MonadHeadlessApp t m => Notify.WatchManager -> FilePath -> m (R.Dynamic t Mode)
getMode watch_manager home = do
  content_event <- FileWatch.watchFileContents watch_manager home ".mode"
  R.holdDyn Klausur $
    content_event <&> \name ->
      find (\mode -> name == Text.toLower (show mode)) modes
        & fromMaybe (error [i|Unknown mode #{name}|])

isDirty :: String -> IO Bool
isDirty gitDir = ((/= "") <$> (git "--no-optional-locks" "-C" gitDir "status" "--porcelain" |> captureTrim)) `catch` (\(_ :: SomeException) -> pure True)

isUnpushed :: String -> IO Bool
isUnpushed gitDir = do
  revs <- tryCmd (git "--no-optional-locks" "-C" gitDir "branch" "-r" "--contains" "HEAD")
  pure $ LBS.null revs

tryCmd :: Proc a -> IO LBS.ByteString
tryCmd x = ignoreFailure x |> captureTrim

newtype Module t m a = Module (m (R.Event t a, IO ()))

eventModule :: forall t m a. R.MonadHeadlessApp t m => m (R.Event t a) -> Module t m a
eventModule = \event_action -> Module $ fmap (,pass) event_action

writeVars :: R.MonadHeadlessApp t m => [R.Event t (Maybe Component)] -> m ()
writeVars vars = do
  writeEvent <-
    vars
      & mapM
        ( R.holdDyn Nothing
            >=> R.holdUniqDyn
            %>> (: [])
        )
      %> mconcat
      %> R.updated
      %>> catMaybes
      %>> reverse
      %>> \components -> do
        let list_components =
              components & mapMaybe \case
                MkComponent{..} ->
                  let
                    content' = reflow (if small then 15 else 10) content
                    small' = if small then "true" else "false"
                   in
                    Just [i|{"color":"#{color}","content":["#{Text.intercalate "\", \"" content'}"],"small":#{small'}}|]
        [i|[#{Text.intercalate "," list_components}]|]
  PublishSocket.publish "components" writeEvent

runModules :: R.MonadHeadlessApp t m => [Module t m (Maybe Component)] -> m ()
runModules modules = do
  (vars, actions) <-
    unzip <$> forM modules \case
      Module module' -> do
        (event, action) <- module'
        pure (event, action)
  void $ liftIO $ Conc.forkIO $ Async.mapConcurrently_ id actions
  writeVars vars

simpleModule :: forall t m a. (R.MonadHeadlessApp t m, Eq a) => Int -> IO a -> Module t m a
simpleModule delay action = eventModule do
  tick <- tickEvent delay
  ReflexUtil.performEventThreaded tick $ const action

simpleModeModule :: forall t m a. (R.MonadHeadlessApp t m, Eq a) => Int -> R.Dynamic t Mode -> (Mode -> IO a) -> Module t m a
simpleModeModule delay mode action = eventModule do
  tick <-
    tickEvent delay
      <&> (\event -> R.leftmost [R.updated mode, R.tag (R.current mode) event])
  ReflexUtil.performEventThreaded tick action

tickEvent :: R.MonadHeadlessApp t m => Int -> m (R.Event t ())
tickEvent delay =
  R.tickLossyFromPostBuildTime (realToFrac delay / realToFrac oneSecond)
    <&> void

withColor :: Monad m => Text -> Text -> m (Maybe Component)
withColor color content = pure $ Just (withColor' color content)

withColor' :: Text -> Text -> Component
withColor' color content = MkComponent{color, content, small = False}

data Component = MkComponent
  { color :: Text
  , content :: Text
  , small :: Bool
  }
  deriving (Eq)

data Warning = MkWarning
  { description :: Text
  , group :: Text
  , subgroup :: Maybe Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)

data WarningGroup = MkWarningGroup
  { name :: Text
  , count :: Int
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)

data Appointment = MkAppointment
  { start :: Text
  , end :: Text
  , title :: Text
  , description :: Text
  , location :: Text
  , calendar :: Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)

when' :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
when' cond result = if cond then result else pure Nothing

red :: Text
red = "F28FAD"
yellow :: Text
yellow = "FAE3B0"
cyan :: Text
cyan = "89DCEB"

reflow :: Int -> Text -> [Text]
reflow width =
  Text.lines
    % map (reflowLine width)
    % join

reflowLine :: Int -> Text -> [Text]
reflowLine width =
  words
    % foldl' go (Nothing, [])
    % \case
      (Just last_line, rest) -> reverse (last_line : rest)
      (Nothing, rest) -> reverse rest
 where
  go (Nothing, rest) word | Text.length word < width = (Just word, rest)
  go (Nothing, rest) too_long_word = go (Nothing, (Text.take width too_long_word : rest)) (Text.drop width too_long_word)
  go (Just line, rest) word
    | new_line <- line <> " " <> word
    , Text.length new_line < width =
        (Just new_line, rest)
  go (Just line, rest) word = go (Nothing, line : rest) word

main :: IO ()
main = Notify.withManager \watch_manager -> do
  missing <-
    missingExecutables
      <&> nonEmpty
  whenJust missing \missing' -> sayErr [i|missing executables #{missing'}|]
  mkdir "-p" PublishSocket.socketsDir
  home <- getEnv "HOME"
  let git_dir = home </> "git"
      modes_dir = home </> ".volatile" </> "modes"
  dirty_var <- newTVarIO []
  R.runHeadlessApp do
    mode <- getMode watch_manager home
    let mk_mode_event = \event -> R.leftmost [R.updated mode, R.tag (R.current mode) event]
    five_seconds_tick <- tickEvent (5 * oneSecond) <&> mk_mode_event
    notmuch_update <-
      FileWatch.watchFile watch_manager (home </> "Maildir/.notmuch/xapian") "flintlock"
        <&> mk_mode_event
    start <- R.getPostBuild
    git_dir_change <- (start <>) . void <$> FileWatch.watchDir watch_manager git_dir False (const True)
    git_dirs_event <- ReflexUtil.performEventThreaded git_dir_change \_ -> listDirectory git_dir
    let git_dirs_event' =
          git_dirs_event <&> \dirs -> do
            dir_update_events <- forM dirs \dir -> do
              -- sub_dirs <-
              --  liftIO (fd "-d2" "-td" "." (git_dir </> dir) |> captureTrim)
              --    <&> decodeUtf8
              --      % String.lines
              --      % take 100
              dir_events <- forM [git_dir </> dir] \sub_dir -> FileWatch.watchDir watch_manager sub_dir False (const True)
              git_dir_event <- FileWatch.watchDir watch_manager (git_dir </> dir </> ".git") False (const True)
              git_refs_event <- FileWatch.watchDir watch_manager (git_dir </> dir </> ".git/refs") True (const True)
              pure $
                mconcat (void <$> dir_events)
                  <> void git_dir_event
                  <> void git_refs_event
                  $> [dir]
            pure $ mconcat dir_update_events
    git_dir_events <- (<> git_dirs_event) . R.switchDyn <$> R.networkHold (pure R.never) git_dirs_event'
    let modules =
          [ simpleModeModule (5 * oneSecond) mode \mode' -> do
              code_updates <- case mode' of
                Code ->
                  exe "software-updates" "-x" "print-unread"
                    & tryCmd
                    % liftIO
                    %> decodeUtf8
                    %> Text.replace " unread articles" ""
                    %> toString
                    %> readMaybe
                    %> fromMaybe 0
                _ -> pure 0
              when' (code_updates /= 0) $ withColor cyan [i|Code Updates: #{code_updates}|]
          , simpleModule (5 * oneSecond) do
              let hosts = ["hera", "fluffy"]
              unreachable_hosts <- flip filterM hosts \host -> isLeft <$> (Shh.tryFailure do (exe "/run/wrappers/bin/ping" "-c" "1" (toString host)) &> Shh.devNull)
              when' ([] /= unreachable_hosts) do withColor red [i|No tunnel to #{Text.intercalate ", " unreachable_hosts}|]
          , simpleModeModule (5 * oneSecond) mode \mode' -> do
              current_kernel <- readlink "/run/current-system/kernel" |> captureTrim
              booted_kernel <- readlink "/run/booted-system/kernel" |> captureTrim
              when' (mode' /= Klausur && current_kernel /= booted_kernel) $ withColor yellow "Booted kernel stale"
          , simpleModeModule (5 * oneSecond) mode \mode' -> do
              behind <-
                if mode' /= Klausur
                  then tryCmd (git "--no-optional-locks" "-C" (git_dir </> "config") "log" "--oneline" "origin/main" "^main")
                  else pure ""
              when' (not $ LBS.null behind) $ withColor yellow [i|Config #{show (length (LBSC.lines behind))} commits behind.|]
          ]
    commit_var <- newTVarIO ""
    system_var <- newTVarIO ""
    modes_var <- newTVarIO ""
    system_dirty_var <- newTVarIO False
    modes_dirty_var <- newTVarIO False
    host_name <- ByteStringChar.strip <$> readFileBS "/etc/hostname"
    let scan = do
          current_commit <- readFileBS (git_dir </> "config/.git/refs/heads/main")
          system_commit <- Exception.try @Exception.IOException do readFileBS "/run/current-system/config-commit"
          modes_commit <- Exception.try do readFileBS (modes_dir </> "config-commit")
          current_system <- readlink "/run/current-system" |> captureTrim
          current_modes <- readlink modes_dir |> captureTrim
          let stale_config = \case
                (Right commit) | commit == current_commit -> False
                _ -> True
              system_stale = stale_config system_commit
              modes_stale = stale_config modes_commit
          (commit_change, system_change, modes_change) <-
            atomically $
              (,,)
                <$> (STM.stateTVar commit_var \previous_commit -> (previous_commit /= current_commit, current_commit))
                <*> (STM.stateTVar system_var \previous_system -> (previous_system /= current_system, current_system))
                <*> (STM.stateTVar modes_var \previous_modes -> (previous_modes /= current_modes, current_modes))
          if system_stale
            then when (commit_change || system_change) do
              sayErr "Eval system config …"
              next_system <- nix "eval" "--raw" ([i|#{home}/git/config\#nixosConfigurations.#{host_name}.config.system.build.toplevel.drvPath|] :: String) |> captureTrim
              sayErr "System eval finished."
              diff_is_small <- diffIsSmall next_system current_system
              atomically $ writeTVar system_dirty_var (not diff_is_small)
            else atomically do writeTVar system_dirty_var False
          if modes_stale
            then when (commit_change || modes_change) do
              sayErr "Eval home config …"
              next_modes <- nix "eval" "--raw" ([i|#{home}/git/config\#homeModes.#{host_name}.drvPath|] :: String) |> captureTrim
              sayErr "Home eval finished."
              diff_is_small <- diffIsSmall next_modes current_modes
              atomically $ writeTVar modes_dirty_var (not diff_is_small)
            else atomically do writeTVar modes_dirty_var False
          system_dirty <- readTVarIO system_dirty_var
          modes_dirty <- readTVarIO modes_dirty_var
          let stale_warn = \scope ->
                MkWarning
                  { description = [i|Current #{scope} stale|]
                  , group = "warning"
                  , subgroup = Nothing
                  }
          pure $ [stale_warn "system" | system_dirty] <> [stale_warn "home" | modes_dirty]
    (stale_warning, stale_trigger) <- R.newTriggerEvent
    void $ liftIO $ Conc.forkIO $ forever do
      Concurrent.threadDelay (4 * oneSecond)
      dirty <- elem "config" <$> readTVarIO dirty_var
      stale_trigger =<< if dirty then pure [] else scan
    dirty_event <- do
      dirty_updates <- ReflexUtil.performEventThreaded git_dir_events \dirs -> do
        now_dirty <- Set.fromList . fmap toText <$> filterM (isDirty . (git_dir </>)) dirs
        pure (Set.difference (Set.fromList (fmap toText dirs)) now_dirty, now_dirty)
      set_of_dirties <- R.foldDyn (\(now_clean, now_dirty) dirty -> Set.union now_dirty (Set.difference dirty now_clean)) mempty dirty_updates
      void $ ReflexUtil.performEventThreaded (R.updated set_of_dirties) \dirty_dirs -> atomically $ writeTVar dirty_var (toList dirty_dirs)
      pure $ R.updated $ R.ffor2 mode set_of_dirties \mode' dirty_dirs' ->
        let dirty_dirs = (if (mode' == Klausur) then Set.filter (== "promotion") else id) dirty_dirs'
         in toList dirty_dirs <&> \dir ->
              MkWarning
                { description = dir
                , group = "git"
                , subgroup = Just "dirty"
                }
    unpushed_event <- do
      dirty_updates <- ReflexUtil.performEventThreaded git_dir_events \dirs -> do
        now_dirty <- Set.fromList . fmap toText <$> filterM (isUnpushed . (git_dir </>)) dirs
        pure (Set.difference (Set.fromList (fmap toText dirs)) now_dirty, now_dirty)
      set_of_dirties <- R.foldDyn (\(now_clean, now_dirty) dirty -> Set.union now_dirty (Set.difference dirty now_clean)) mempty dirty_updates
      pure $ R.updated $ R.ffor2 mode set_of_dirties \mode' dirty_dirs' ->
        let dirty_dirs = (if (mode' == Klausur) then Set.filter (== "promotion") else id) dirty_dirs'
         in toList dirty_dirs <&> \dir ->
              MkWarning
                { description = dir
                , group = "git"
                , subgroup = Just "unpushed"
                }
    tasks_update <- FileWatch.watchFile watch_manager (home </> ".task") "pending.data" <&> mk_mode_event
    inbox_event <-
      ReflexUtil.performEventThreaded tasks_update $
        \case
          Orga ->
            ( Maralorn.Taskwarrior.getInbox <<&>> \task ->
                MkWarning
                  { description = task.description <> maybe "" (\num -> [i| (#{num})|]) task.id
                  , group = "inbox"
                  , subgroup = Nothing
                  }
            )
          _ -> pure []
    let process_notmuch_description =
          Text.splitOn "["
            % drop 1
            % Text.intercalate "["
            % ("[" <>)
            % Text.splitOn "("
            % reverse
            % drop 1
            % reverse
            % Text.intercalate "("
            % Text.replace "\"" ""
    mail_unread_event <-
      ReflexUtil.performEventThreaded
        notmuch_update
        \case
          Klausur -> pure []
          _ -> Text.lines . decodeUtf8 <$> (notmuch "search" "folder:hera/Inbox" "tag:unread" |> captureTrim)
        <<&>> fmap
          ( \msg ->
              MkWarning
                { description = process_notmuch_description msg
                , group = "inbox"
                , subgroup = Just "e-mail"
                }
          )
    mail_inbox_event <-
      ReflexUtil.performEventThreaded
        notmuch_update
        \case
          Orga -> Text.lines . decodeUtf8 <$> (notmuch "search" "folder:hera/Inbox" "not" "tag:unread" |> captureTrim)
          _ -> pure []
        <<&>> fmap
          ( \msg ->
              MkWarning
                { description = process_notmuch_description msg
                , group = "inbox"
                , subgroup = Just "e-mail-open"
                }
          )
    mail_code_event <-
      ReflexUtil.performEventThreaded
        notmuch_update
        \case
          Code -> Text.lines . decodeUtf8 <$> (notmuch "search" "folder:hera/Code" |> captureTrim)
          _ -> pure []
        <<&>> fmap
          ( \msg ->
              MkWarning
                { description = process_notmuch_description msg
                , group = "inbox"
                , subgroup = Just "Code"
                }
          )
    warnings <- ReflexUtil.concatEvents [dirty_event, unpushed_event, stale_warning, mail_unread_event, mail_inbox_event, mail_code_event, inbox_event]
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
    PublishSocket.publish "mode" (mk_mode_event start <&> show)
    player_events <- Player.playerModule home
    appointments_event <- ReflexUtil.performEventThreaded five_seconds_tick $ const do
      appointments <-
        decodeUtf8
          <$> tryCmd
            ( khal
                [ "list"
                , "-o"
                , "-a"
                , "Standard"
                , "-a"
                , "Planung"
                , "-a"
                , "Uni"
                , "-a"
                , "Maltaire"
                , "now"
                , "2h"
                , "-df"
                , ""
                , "-f"
                , "@=@{start}@@@{end}@@@{title}@@@{description}@@@{location}@@@{calendar}"
                ]
            )
      pure $
        appointments
          & Text.splitOn "@=@"
          %> Text.splitOn "@@@"
          %>> cleanString
          % mapMaybe \case
            [start', end, title, description, location, calendar] ->
              Just $
                MkAppointment
                  { start = start'
                  , end
                  , title
                  , description
                  , location
                  , calendar
                  }
            _ -> Nothing
    PublishSocket.publishJson "calendar" appointments_event
    PublishSocket.publishJson "players" player_events
    timer_events <- Timer.timers watch_manager
    PublishSocket.publishJson "timers" timer_events
    audio_event <- Audio.audioUpdateEvent
    bluetooth_events <- Bluetooth.bluetooth audio_event
    PublishSocket.publishJson "bluetooth" bluetooth_events
    runModules modules
    pure R.never -- We have no exit condition.

cleanString :: Text -> Text
cleanString = Text.replace "\"" "" . Text.intercalate "\\n" . Text.lines . Text.strip

diffIsSmall :: LBSC.ByteString -> LBSC.ByteString -> IO Bool
diffIsSmall = \pathA pathB -> (== "[]") <$> (nix_diff "--json" [pathA, pathB] |> jq ".inputsDiff.inputDerivationDiffs" |> captureTrim)
