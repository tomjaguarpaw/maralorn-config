{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch)
import Control.Exception qualified as Exception

import Data.ByteString.Char8 qualified as ByteStringChar
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC

import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Relude
import Say (say)
import Shh (ExecReference (Absolute), Proc, captureTrim, exe, ignoreFailure, load, readInputLines, (&>), (|>))
import Shh qualified

import Control.Concurrent qualified as Conc
import Data.List qualified as String
import Data.Set qualified as Set
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Reflex.Network qualified as R
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FSNotify qualified as Notify
import System.FilePath ((</>))

infixl 9 %
(%) :: (a -> b) -> (b -> c) -> a -> c
f % g = g . f

infixl 9 %>
(%>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f %> g = fmap g . f

infixl 9 %>>
(%>>) :: (Functor g, Functor f) => (a -> f (g b)) -> (b -> c) -> a -> f (g c)
f %>> g = fmap (fmap g) . f

infixl 1 <<&>>
(<<&>>) :: (Functor g, Functor f) => f (g a) -> (a -> b) -> f (g b)
x <<&>> g = fmap (fmap g) x

data Mode = Klausur | Orga | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

load Absolute ["git", "khal", "playerctl", "notmuch", "readlink", "nix", "nix-diff", "jq"]

missingExecutables :: IO [FilePath]
modes :: [Mode]
modes = enumFrom Klausur

getMode :: R.MonadHeadlessApp t m => Notify.WatchManager -> FilePath -> m (R.Dynamic t Mode)
getMode watch_manager home = do
  content_event <- watchFileContents watch_manager home ".mode"
  R.holdDyn Klausur $
    content_event <&> \name ->
      find (\mode -> name == Text.toLower (show mode)) modes
        & fromMaybe (error [i|Unknown mode #{name}|])

hush :: Either a1 a2 -> Maybe a2
hush = \case
  Left _ -> Nothing
  Right x -> Just x

watchDir :: R.MonadHeadlessApp t m => Notify.WatchManager -> FilePath -> Bool -> Notify.ActionPredicate -> m (R.Event t Notify.Event)
watchDir watch_manager path recursive predicate = do
  let watch = if recursive then Notify.watchTree else Notify.watchDir
  R.newEventWithLazyTriggerWithOnComplete \callback -> do
    finish_callback <- newEmptyTMVarIO
    void $ Async.async do
      cb <- watch watch_manager path predicate (`callback` pass)
      atomically $ putTMVar finish_callback cb
    pure $ void $ Async.async $ join $ atomically $ takeTMVar finish_callback

watchFile :: R.MonadHeadlessApp t m => Notify.WatchManager -> FilePath -> FilePath -> m (R.Event t ())
watchFile watch_manager dir file = do
  start <- R.getPostBuild
  watchDir
    watch_manager
    dir
    False
    (Notify.eventPath % String.isSuffixOf file)
    <&> void
      % (<> start)

watchFileContents :: R.MonadHeadlessApp t m => Notify.WatchManager -> FilePath -> FilePath -> m (R.Event t Text)
watchFileContents watch_manager dir file = do
  event_event <- watchFile watch_manager dir file
  content_event <- performEventThreaded event_event \_ ->
    readFileBS (dir </> file)
      & Exception.try @Exception.IOException
      <&> either
        (const Nothing)
        ( ByteStringChar.strip
            % decodeUtf8Strict @Text
            % hush
        )
  stored_event <- R.holdDyn Nothing content_event
  R.holdUniqDyn stored_event
    <&> R.updated
      % R.fmapMaybe id

isDirty :: String -> IO Bool
isDirty gitDir = ((/= "") <$> (git "--no-optional-locks" "-C" gitDir "status" "--porcelain" |> captureTrim)) `catch` (\(_ :: SomeException) -> pure True)

isUnpushed :: String -> IO Bool
isUnpushed gitDir = do
  revs <- tryCmd (git "--no-optional-locks" "-C" gitDir "branch" "-r" "--contains" "HEAD")
  pure $ LBS.null revs

tryCmd :: Proc a -> IO LBS.ByteString
tryCmd x = ignoreFailure x |> captureTrim

data Module t m a
  = OldModule ((a -> IO ()) -> IO Void)
  | Module (m (R.Event t a, IO ()))

eventModule :: forall t m a. R.MonadHeadlessApp t m => m (R.Event t a) -> Module t m a
eventModule = \event_action -> Module $ fmap (,pass) event_action

separator :: Text
separator = " "

writeVars :: R.MonadHeadlessApp t m => [R.Event t (Maybe Text)] -> m ()
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
      %>> Text.intercalate separator
      %>> writeFileText "/run/user/1000/status-bar"
  R.performEvent_ writeEvent

runModules :: R.MonadHeadlessApp t m => [Module t m (Maybe Text)] -> m ()
runModules modules = do
  (vars, actions) <-
    unzip <$> forM modules \case
      OldModule module' -> do
        (event, trigger) <- R.newTriggerEvent
        pure (event, void $ module' trigger)
      Module module' -> do
        (event, action) <- module'
        pure (event, action)
  void $ liftIO $ Conc.forkIO $ Async.mapConcurrently_ id actions
  writeVars vars

oneSecond :: Int
oneSecond = 1000000

simpleModule :: forall t m a. (R.MonadHeadlessApp t m, Eq a) => Int -> IO a -> Module t m a
simpleModule delay action = eventModule do
  tick <- tickEvent delay
  performEventThreaded tick $ const action

simpleModeModule :: forall t m a. (R.MonadHeadlessApp t m, Eq a) => Int -> R.Dynamic t Mode -> (Mode -> IO a) -> Module t m a
simpleModeModule delay mode action = eventModule do
  tick <-
    tickEvent delay
      <&> (\event -> R.leftmost [R.updated mode, R.tag (R.current mode) event])
  performEventThreaded tick action

tickEvent :: R.MonadHeadlessApp t m => Int -> m (R.Event t ())
tickEvent delay =
  R.tickLossyFromPostBuildTime (realToFrac delay / realToFrac oneSecond)
    <&> void

withColor :: Monad m => Text -> Text -> m (Maybe Text)
withColor color content = pure $ Just (withColor' color content)

withColor' :: Text -> Text -> Text
withColor' color content = [i|${color \##{color}}#{content}|]

when' :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
when' cond result = if cond then result else pure Nothing

playerCTLFormat :: String
playerCTLFormat = "@{{status}} {{title}} | {{album}} | {{artist}}"

data EventRunnerState a = Idle | Running | NextWaiting a

-- Call IO action in a separate thread. If multiple events fire never run two actions in parallel and if more than one action queues up, only run the latest.
performEventThreaded :: R.MonadHeadlessApp t m => R.Event t a -> (a -> IO b) -> m (R.Event t b)
performEventThreaded event action = do
  runnerState <- liftIO $ newTVarIO Idle
  R.performEventAsync $
    event <&> \input callback -> liftIO do
      let runner input' = do
            action input' >>= callback
            next_input <- atomically $ STM.stateTVar runnerState \case
              Idle -> error "Runner should not be in idle state when finishing"
              Running -> (Nothing, Idle)
              NextWaiting next_input -> (Just next_input, Running)
            next_input & maybe pass runner
      run <- atomically $ STM.stateTVar runnerState \case
        Idle -> (True, Running)
        Running -> (False, NextWaiting input)
        NextWaiting{} -> (False, NextWaiting input)
      when run $ void $ Async.async $ runner input

playerModule :: forall t m. R.MonadHeadlessApp t m => FilePath -> Module t m (Maybe Text)
playerModule home = Module do
  (event, trigger) <- R.newTriggerEvent
  pure (event, listenToPlayer trigger)
 where
  update_lines = \trigger -> mapM_ \update -> do
    mpdris_config <-
      [i|#{home}/.config/mpDris2/mpDris2.conf|]
        & readFileBS
          % Exception.try @Exception.IOException
    update
      & decodeUtf8
        % Text.splitOn " | "
        % (get_host mpdris_config <>)
        % filter (Text.null % not)
        % Text.intercalate " "
        % Text.replace "@Stopped" "⏹"
        % Text.replace "@Playing" "▶"
        % Text.replace "@Paused" "⏸"
        % ("\n${alignr}" <>)
        % withColor white
        % runIdentity
        % trigger
  listenToPlayer = \trigger ->
    forever do
      Conc.threadDelay oneSecond
      ignoreFailure
        ( playerctl "metadata" "-F" "-f" playerCTLFormat
            |> Shh.readInputLines (update_lines trigger)
        )
  get_host =
    fromRight ""
      % decodeUtf8
      % lines
      % mapMaybe (Text.stripPrefix "host = ")
      % find (/= "::")
      % maybeToList

red :: Text
red = "F28FAD"

-- green :: String
-- green = "ABE9B3"
yellow :: Text
yellow = "FAE3B0"
blue :: Text
blue = "96CDFB"
magenta :: Text
magenta = "F5C2E7"
cyan :: Text
cyan = "89DCEB"
white :: Text
white = "D9E0EE"

main :: IO ()
main = Notify.withManager \watch_manager -> do
  Nothing <-
    missingExecutables
      <&> nonEmpty
  home <- getEnv "HOME"
  let git_dir = home </> "git"
      modes_dir = home </> ".volatile" </> "modes"
  dirty_var <- newTVarIO []
  R.runHeadlessApp do
    mode <- getMode watch_manager home
    let mk_mode_event = \event -> R.leftmost [R.updated mode, R.tag (R.current mode) event]
    notmuch_update <-
      watchFile watch_manager (home </> "Maildir/.notmuch/xapian") "flintlock"
        <&> mk_mode_event
    start <- R.getPostBuild
    git_dir_change <- (start <>) . void <$> watchDir watch_manager git_dir False (const True)
    git_dirs_event <- performEventThreaded git_dir_change \_ -> listDirectory git_dir
    let git_dirs_event' =
          git_dirs_event <&> \dirs -> do
            dir_update_events <- forM dirs \dir -> do
              -- sub_dirs <-
              --  liftIO (fd "-d2" "-td" "." (git_dir </> dir) |> captureTrim)
              --    <&> decodeUtf8
              --      % String.lines
              --      % take 100
              dir_events <- forM [git_dir </> dir] \sub_dir -> watchDir watch_manager sub_dir False (const True)
              git_dir_event <- watchDir watch_manager (git_dir </> dir </> ".git") False (const True)
              git_refs_event <- watchDir watch_manager (git_dir </> dir </> ".git/refs") True (const True)
              pure $
                mconcat (void <$> dir_events)
                  <> void git_dir_event
                  <> void git_refs_event
                  $> [dir]
            pure $ mconcat dir_update_events
    git_dir_events <- (<> git_dirs_event) . R.switchDyn <$> R.networkHold (pure R.never) git_dirs_event'
    let modules =
          [ simpleModule (1 * oneSecond) do
              now <- Time.getCurrentTime
              notifications <- processNotifications . fromRight "" <$> Exception.try @Exception.IOException (readFileBS [i|#{home}/.notifications/#{Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" now}.log|])
              when' (not $ Text.null notifications) $ withColor red ("\n" <> notifications)
          , simpleModule (5 * oneSecond) $ do
              appointments <- lines . decodeUtf8 <$> tryCmd (khal ["list", "-a", "Standard", "-a", "Planung", "-a", "Uni", "-a", "Maltaire", "now", "2h", "-df", ""])
              when' (not $ null appointments) $
                withColor magenta ("\n${alignr}" <> Text.intercalate " " appointments)
          , playerModule home
          , eventModule do
              performEventThreaded
                notmuch_update
                \case
                  mode' | mode' >= Orga -> notmuch "count" "folder:hera/Inbox" "tag:unread" |> captureTrim
                  _ -> pure "0"
                <<&>> \unread ->
                  [i|Unread: #{unread}|]
                    & withColor red
                    & when' (unread /= "0")
                    & runIdentity
          , eventModule do
              performEventThreaded
                notmuch_update
                \case
                  mode' | mode' >= Leisure -> notmuch "count" "folder:hera/Inbox" |> captureTrim
                  _ -> pure "0"
                <<&>> \inbox ->
                  [i|Inbox: #{inbox}|]
                    & withColor yellow
                    & when' (inbox /= "0")
                    & runIdentity
          , eventModule do
              performEventThreaded
                notmuch_update
                \case
                  Code -> notmuch "count" "folder:hera/Code" |> captureTrim
                  _ -> pure "0"
                <<&>> \code_mails ->
                  [i|Code Mails: #{code_mails}|]
                    & withColor blue
                    & when' (code_mails /= "0")
                    & runIdentity
          , simpleModeModule (5 * oneSecond) mode \mode' -> do
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
          , eventModule do
              dirty_updates <- performEventThreaded git_dir_events \dirs -> do
                now_dirty <- Set.fromList . fmap toText <$> filterM (isDirty . (git_dir </>)) dirs
                pure (Set.difference (Set.fromList (fmap toText dirs)) now_dirty, now_dirty)
              set_of_dirties <- R.foldDyn (\(now_clean, now_dirty) dirty -> Set.union now_dirty (Set.difference dirty now_clean)) mempty dirty_updates
              void $ performEventThreaded (R.updated set_of_dirties) \dirty_dirs -> atomically $ writeTVar dirty_var (toList dirty_dirs)
              pure $ R.updated $ R.ffor2 mode set_of_dirties \mode' dirty_dirs ->
                [i|Dirty: #{Text.intercalate " " (toList dirty_dirs)}|]
                  & withColor red
                  & when' (mode' /= Klausur && not (Set.null dirty_dirs))
                  & runIdentity
          , eventModule do
              dirty_updates <- performEventThreaded git_dir_events \dirs -> do
                now_dirty <- Set.fromList . fmap toText <$> filterM (isUnpushed . (git_dir </>)) dirs
                pure (Set.difference (Set.fromList (fmap toText dirs)) now_dirty, now_dirty)
              set_of_dirties <- R.foldDyn (\(now_clean, now_dirty) dirty -> Set.union now_dirty (Set.difference dirty now_clean)) mempty dirty_updates
              pure $ R.updated $ R.ffor2 mode set_of_dirties \mode' dirty_dirs ->
                [i|Unpushed: #{Text.intercalate " " (toList dirty_dirs)}|]
                  & withColor yellow
                  & when' (mode' /= Klausur && not (Set.null dirty_dirs))
                  & runIdentity
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
          , OldModule \var -> do
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
                        say "Eval system config …"
                        next_system <- nix "eval" "--raw" ([i|#{home}/git/config\#nixosConfigurations.#{host_name}.config.system.build.toplevel.drvPath|] :: String) |> captureTrim
                        say "System eval finished."
                        diff_is_small <- diffIsSmall next_system current_system
                        atomically $ writeTVar system_dirty_var (not diff_is_small)
                      else atomically do writeTVar system_dirty_var False
                    if modes_stale
                      then when (commit_change || modes_change) do
                        say "Eval home config …"
                        next_modes <- nix "eval" "--raw" ([i|#{home}/git/config\#homeModes.#{host_name}.drvPath|] :: String) |> captureTrim
                        say "Home eval finished."
                        diff_is_small <- diffIsSmall next_modes current_modes
                        atomically $ writeTVar modes_dirty_var (not diff_is_small)
                      else atomically do writeTVar modes_dirty_var False
                    system_dirty <- readTVarIO system_dirty_var
                    modes_dirty <- readTVarIO modes_dirty_var
                    when' (system_dirty || modes_dirty) $ withColor yellow [i|Current #{case (system_dirty,modes_dirty) of (True, True) -> "home and system"; (True, _) -> "system"; _ -> "home"} stale|]
              forever do
                Concurrent.threadDelay (4 * oneSecond)
                dirty <- elem "config" <$> readTVarIO dirty_var
                var =<< if dirty then pure Nothing else scan
          , eventModule . pure $
              mk_mode_event R.never
                <&> show
                  % withColor blue
                  % runIdentity
          ]
    runModules modules
    pure R.never -- We have no exit condition.

processNotifications :: ByteString -> Text
processNotifications =
  Text.intercalate [i|\n${color \##{red}}|]
    . toList
    . Set.fromList
    . filter (\x -> not $ any (`Text.isPrefixOf` x) notificationBlockList)
    . filter (not . Text.null)
    . fmap
      ( Text.replace "&gt;" ">"
          . Text.replace "&lt;" "<"
          . Text.replace "#" "\\#"
          . Text.intercalate ":${color0} "
      )
    . filter (\x -> length x >= 2)
    . fmap (filter (not . Text.null) . drop 3 . Text.splitOn "|")
    . foldl'
      ( flip \line -> \case
          [] -> [line]
          messages | Text.isInfixOf "|" line -> line : messages
          last_message : rest_of_messages -> last_message <> " " <> line : rest_of_messages
      )
      []
    . lines
    . decodeUtf8
    . ByteStringChar.strip

notificationBlockList :: [Text]
notificationBlockList = ["Automatic suspend", "Auto suspend"]

diffIsSmall :: LBSC.ByteString -> LBSC.ByteString -> IO Bool
diffIsSmall = \pathA pathB -> (== "[]") <$> (nix_diff "--json" [pathA, pathB] |> jq ".inputsDiff.inputDerivationDiffs" |> captureTrim)
