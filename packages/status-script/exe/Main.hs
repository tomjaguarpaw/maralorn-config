module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch, onException)
import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Unique qualified as Unique
import Relude
import Say (say, sayErr)
import Shh (ExecReference (Absolute), Proc, captureTrim, exe, ignoreFailure, load, readInputLines, (&>), (|>))
import Shh qualified
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))

data Mode = Klausur | Orga | Communication | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

load Absolute ["git", "khal", "playerctl", "notmuch", "readlink", "nix", "nix-diff", "jq", "tailscale"]

modes :: [Mode]
modes = enumFrom Klausur

getMode :: FilePath -> IO Mode
getMode home = do
  let mode_file = home </> ".mode"
  name <- decodeUtf8 . ByteString.strip <$> readFileBS mode_file `onException` sayErr [i|File #{mode_file} not found.|]
  maybe (sayErr [i|Unknown mode #{name}|] >> error [i|Unknown mode #{name}|]) pure $ find (\mode -> name == Text.toLower (show mode)) modes

isDirty :: String -> IO Bool
isDirty gitDir = ((/= "") <$> (git "--no-optional-locks" "-C" gitDir "status" "--porcelain" |> captureTrim)) `catch` (\(_ :: SomeException) -> pure True)

isUnpushed :: String -> IO Bool
isUnpushed gitDir = do
  revs <- tryCmd (git "--no-optional-locks" "-C" gitDir "branch" "-r" "--contains" "HEAD")
  pure $ LBS.null revs

tryCmd :: Proc a -> IO LBS.ByteString
tryCmd x = ignoreFailure x |> captureTrim

newtype Var a = MkVar
  { value :: TVar (a, Unique.Unique)
  }

getUnique = fmap snd . readTVar . value
getVal = fmap fst . readTVar . value

newVar :: a -> IO (Var a)
newVar initial_value = do
  unique <- Unique.newUnique
  MkVar <$> newTVarIO (initial_value, unique)

newMaybeVar :: IO (Var (Maybe a))
newMaybeVar = newVar Nothing

type Vars = [Var (Maybe Text)]
type Module a = Var a -> IO Void

separator = "\n$color1$hr\n"

writeVars :: Vars -> IO Void
writeVars vars = onUpdatesMono vars do
  outputs <- atomically $ mapM getVal vars
  writeFileText "/run/user/1000/status-bar" $
    Text.intercalate separator $
      reverse $
        catMaybes outputs

data AnyVar where
  AVar :: Var a -> AnyVar

onUpdate :: Var a -> (a -> IO ()) -> IO Void
onUpdate var action = onUpdatesMono [var] do
  action =<< atomically (getVal var)

onUpdatesMono :: [Var a] -> IO () -> IO Void
onUpdatesMono = onUpdates . fmap AVar

onUpdates :: [AnyVar] -> IO () -> IO Void
onUpdates vars action = go []
 where
  go previous_uniques = do
    next_uniques <- atomically $ do
      current_uniques <- mapM (\(AVar a) -> getUnique a) vars
      STM.check $ previous_uniques /= current_uniques
      pure current_uniques
    action
    go next_uniques

runModules :: [Module (Maybe Text)] -> IO ()
runModules modules = do
  (vars, actions) <-
    unzip <$> forM modules \module' -> do
      var <- newMaybeVar
      pure (var, module' var)
  foldConcurrently_ (writeVars vars : actions)

foldConcurrently_ :: Foldable f => f (IO a) -> IO ()
foldConcurrently_ = Async.mapConcurrently_ id

oneSecond :: Int
oneSecond = 1000000

updateVarIfChanged :: Eq a => Var a -> a -> IO ()
updateVarIfChanged var@MkVar{value} new_value = do
  new_unique <- Unique.newUnique
  atomically do
    old_value <- getVal var
    unless (old_value == new_value) do
      writeTVar value (new_value, new_unique)

simpleModule :: Eq a => Int -> IO a -> Module a
simpleModule delay action var = forever do
  updateVarIfChanged var =<< action
  Concurrent.threadDelay delay

withColor :: Monad m => Text -> Text -> m (Maybe Text)
withColor color content = pure $ Just [i|${color \##{color}}#{content}|]

when' :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
when' cond result = if cond then result else pure Nothing

playerCTLFormat :: String
playerCTLFormat = "@{{status}} {{title}} | {{album}} | {{artist}}"

playerModule :: Module (Maybe Text)
playerModule = \var ->
  let update_lines =
        mapM_
          ( updateVarIfChanged var
              . runIdentity
              . withColor white
              . Text.replace "@Stopped" "⏹"
              . Text.replace "@Playing" "▶"
              . Text.replace "@Paused" "⏸"
              . Text.intercalate "\n"
              . filter (not . Text.null)
              . Text.splitOn " | "
              . decodeUtf8
          )
   in forever $
        playerctl "metadata" "-F" "-f" playerCTLFormat
          |> Shh.readInputLines update_lines

black = "6E6C7E"
red = "F28FAD"
green = "ABE9B3"
yellow = "FAE3B0"
blue = "96CDFB"
magenta = "F5C2E7"
cyan = "89DCEB"
white = "D9E0EE"

main :: IO ()
main = do
  home <- getEnv "HOME"
  let git_dir = home </> "git"
      modes_dir = home </> ".volatile" </> "modes"
  mode_var <- newVar Unrestricted
  dirty_var <- newTVarIO []
  let read_mode = fst <$> readTVarIO (value mode_var)
      modules =
        [ simpleModule (5 * oneSecond) $ do
            appointments <- lines . decodeUtf8 <$> tryCmd (khal ["list", "-a", "Standard", "-a", "Planung", "-a", "Uni", "-a", "Maltaire", "now", "2h", "-df", ""])
            when' (not $ null appointments) $
              withColor magenta (Text.unlines appointments)
        , playerModule
        , simpleModule oneSecond $ do
            mode <- read_mode
            unread <-
              if mode >= Orga
                then notmuch "count" "folder:hera/Inbox" "tag:unread" |> captureTrim
                else pure "0"
            when' (unread /= "0") $ withColor red [i|Unread: #{unread}|]
        , simpleModule oneSecond $ do
            mode <- read_mode
            inbox <-
              if mode == Leisure
                then notmuch "count" "folder:hera/Inbox" |> captureTrim
                else pure "0"
            when' (inbox /= "0") $ withColor yellow [i|Inbox: #{inbox}|]
        , simpleModule oneSecond $ do
            mode <- read_mode
            codeMails <-
              if mode == Code
                then notmuch "count" "folder:hera/Code" |> captureTrim
                else pure "0"
            when' (codeMails /= "0") $ withColor blue [i|Code Mails: #{codeMails}|]
        , simpleModule (5 * oneSecond) $ do
            mode <- read_mode
            codeUpdates <-
              if mode == Code
                then fromMaybe 0 . readMaybe . toString . Text.replace " unread articles" "" . decodeUtf8 <$> tryCmd (exe "software-updates" "-x" "print-unread")
                else pure 0
            when' (codeUpdates /= 0) $ withColor cyan [i|Code Updates: #{codeUpdates}|]
        , simpleModule (5 * oneSecond) do
            dirs <- listDirectory git_dir
            dirty <- fmap toText <$> filterM (isDirty . (git_dir </>)) dirs
            atomically $ writeTVar dirty_var dirty
            when' (not $ null dirty) $ withColor red [i|Dirty: #{Text.intercalate " " dirty}|]
        , simpleModule (5 * oneSecond) do
            dirs <- listDirectory git_dir
            unpushed <- fmap toText <$> filterM (isUnpushed . (git_dir </>)) dirs
            when' (not $ null unpushed) do withColor yellow [i|Unpushed: #{Text.intercalate " " unpushed}|]
        , simpleModule (5 * oneSecond) do
            let hosts = ["hera", "fluffy"]
            unreachable_hosts <- flip filterM hosts \host -> isLeft <$> (Shh.tryFailure do (tailscale "ping" "-c" "1" (toString host)) &> Shh.devNull)
            when' ([] /= unreachable_hosts) do withColor red [i|No tunnel to #{Text.intercalate ", " unreachable_hosts}|]
        , \var -> do
            commit_var <- newTVarIO ""
            system_var <- newTVarIO ""
            modes_var <- newTVarIO ""
            system_dirty_var <- newTVarIO False
            modes_dirty_var <- newTVarIO False
            host_name <- ByteString.strip <$> readFileBS "/etc/hostname"
            let scan = do
                  current_commit <- readFileBS (git_dir </> "config/.git/refs/heads/main")
                  system_commit <- Exception.try do readFileBS "/run/current-system/config-commit"
                  modes_commit <- Exception.try do readFileBS (modes_dir </> "config-commit")
                  current_system <- readlink "/run/current-system" |> captureTrim
                  current_modes <- readlink modes_dir |> captureTrim
                  let stale_config :: Either Exception.IOException ByteString -> Bool = \case
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
                      next_system <- nix "eval" "--raw" ([i|/home/maralorn/git/config\#nixosConfigurations.#{host_name}.config.system.build.toplevel.drvPath|] :: String) |> captureTrim
                      say "System eval finished."
                      diff_is_small <- diffIsSmall next_system current_system
                      atomically $ writeTVar system_dirty_var (not diff_is_small)
                    else atomically do writeTVar system_dirty_var False
                  if modes_stale
                    then when (commit_change || modes_change) do
                      say "Eval home config …"
                      next_modes <- nix "eval" "--raw" ([i|/home/maralorn/git/config\#homeModes.#{host_name}.drvPath|] :: String) |> captureTrim
                      say "Home eval finished."
                      diff_is_small <- diffIsSmall next_modes current_modes
                      atomically $ writeTVar modes_dirty_var (not diff_is_small)
                    else atomically do writeTVar modes_dirty_var False
                  system_dirty <- readTVarIO system_dirty_var
                  modes_dirty <- readTVarIO modes_dirty_var
                  when' (system_dirty || modes_dirty) $ withColor yellow [i|Current #{case (system_dirty,modes_dirty) of (True, True) -> "home and system"; (True, _) -> "system"; _ -> "home"} stale|]
            var & simpleModule oneSecond do
              Concurrent.threadDelay (4 * oneSecond)
              dirty <- elem "config" <$> readTVarIO dirty_var
              if dirty then pure Nothing else scan
        , \var ->
            onUpdate mode_var $ updateVarIfChanged var . runIdentity . withColor blue . show
        , simpleModule (1 * oneSecond) do
            now <- Time.getCurrentTime
            notifications <- processNotifications . fromRight "" <$> Exception.try @Exception.IOException (readFileBS [i|/home/maralorn/.notifications/#{Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" now}.log|])
            when' (not $ Text.null notifications) $ withColor red notifications
        ]
  foldConcurrently_
    [ void $ simpleModule oneSecond (getMode home) mode_var
    , runModules modules
    ]

processNotifications =
  Text.intercalate [i|\n$color1$hr${color \##{red}}\n|]
    . filter (\x -> not $ any (`Text.isPrefixOf` x) notificationBlockList)
    . filter (not . Text.null)
    . fmap
      ( Text.replace "&gt;" ">"
          . Text.replace "&lt;" "<"
          . Text.intercalate ":${color0} "
      )
    . filter (\x -> length x >= 2)
    . fmap (filter (not . Text.null) . drop 3 . Text.splitOn "|")
    . foldl'
      ( flip \line -> \case
          [] -> [line]
          messages | Text.isInfixOf "|" line -> line : messages
          last_message : rest_of_messages -> last_message <> "\n" <> line : rest_of_messages
      )
      []
    . lines
    . decodeUtf8
    . ByteString.strip

notificationBlockList = ["Automatic suspend", "Auto suspend"]

diffIsSmall = \pathA pathB -> (== "[]") <$> (nix_diff "--json" [pathA, pathB] |> jq ".inputsDiff.inputDerivationDiffs" |> captureTrim)
