module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch, onException)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Unique qualified as Unique
import Relude
import Say (sayErr)
import Shh (ExecReference (Absolute), Proc, captureTrim, exe, ignoreFailure, load, readInputLines, (|>))
import System.Directory (listDirectory)

data Mode = Klausur | Orga | Communication | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

load Absolute ["git", "khal", "playerctl", "notmuch", "readlink", "nix"]

modes :: [Mode]
modes = enumFrom Klausur

getMode :: IO Mode
getMode = do
  name <- decodeUtf8 . ByteString.strip <$> readFileBS "/home/maralorn/.mode" `onException` sayErr "File /home/maralorn/.mode not found."
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

writeVars :: Vars -> IO Void
writeVars vars = onUpdatesMono vars do
  outputs <- atomically $ mapM getVal vars
  writeFileText "/run/user/1000/status-bar" $
    Text.replace "&" "&amp;" $
      Text.unwords $
        ("<executor.markup.true>" :) $
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
withColor color content = pure $ Just [i|<span foreground='\##{color}'>#{content}</span>|]

when' :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
when' cond result = if cond then result else pure Nothing

playerCTLFormat :: String
playerCTLFormat = "@{{status}} {{title}} - {{album}} - {{artist}}"

playerModule :: Module (Maybe Text)
playerModule = \var ->
  let update_lines =
        mapM_
          ( updateVarIfChanged var
              . Just
              . Text.replace "@Stopped" "⏹"
              . Text.replace "@Playing" "▶"
              . Text.replace "@Paused" "⏸"
              . Text.replace "-  -" "-"
              . decodeUtf8
          )
   in forever $
        playerctl "metadata" "-F" "-f" playerCTLFormat
          |> Shh.readInputLines update_lines

main :: IO ()
main = do
  mode_var <- newVar Unrestricted
  let read_mode = fst <$> readTVarIO (value mode_var)
      modules =
        [ simpleModule (5 * oneSecond) $ do
            appointments <- lines . decodeUtf8 <$> tryCmd (khal ["list", "-a", "Standard", "-a", "Planung", "-a", "Uni", "-a", "Maltaire", "now", "2h", "-df", ""])
            when' (not $ null appointments) $
              withColor "8839ef" (Text.intercalate "; " appointments)
        , playerModule
        , simpleModule oneSecond $ do
            mode <- read_mode
            unread <-
              if mode >= Orga
                then notmuch "count" "folder:hera/Inbox" "tag:unread" |> captureTrim
                else pure "0"
            when' (unread /= "0") $ withColor "d20f39" [i|Unread: #{unread}|]
        , simpleModule oneSecond $ do
            mode <- read_mode
            inbox <-
              if mode == Leisure
                then notmuch "count" "folder:hera/Inbox" |> captureTrim
                else pure "0"
            when' (inbox /= "0") $ withColor "e53443" [i|Inbox: #{inbox}|]
        , simpleModule oneSecond $ do
            mode <- read_mode
            codeMails <-
              if mode == Code
                then notmuch "count" "folder:hera/Code" |> captureTrim
                else pure "0"
            when' (codeMails /= "0") $ withColor "8839ef" [i|Code Mails: #{codeMails}|]
        , simpleModule (5 * oneSecond) $ do
            mode <- read_mode
            codeUpdates <-
              if mode == Code
                then fromMaybe 0 . readMaybe . toString . Text.replace " unread articles" "" . decodeUtf8 <$> tryCmd (exe "software-updates" "-x" "print-unread")
                else pure 0
            when' (codeUpdates /= 0) $ withColor "179299" [i|Code Updates: #{codeUpdates}|]
        , simpleModule (5 * oneSecond) $ do
            dirs <- listDirectory "/home/maralorn/git"
            dirty <- fmap toText <$> filterM (isDirty . ("/home/maralorn/git/" <>)) dirs
            when' (not $ null dirty) $ withColor "e64443" [i|Dirty: #{Text.intercalate " " dirty}|]
        , simpleModule (5 * oneSecond) $ do
            dirs <- listDirectory "/home/maralorn/git"
            unpushed <- fmap toText <$> filterM (isUnpushed . ("/home/maralorn/git/" <>)) dirs
            when' (not $ null unpushed) $ withColor "fe640b" [i|Unpushed: #{Text.intercalate " " unpushed}|]
        , simpleModule (60 * oneSecond) $ do
            current_kernel <- readlink "/run/current-system/kernel" |> captureTrim
            booted_kernel <- readlink "/run/booted-system/kernel" |> captureTrim
            when' (current_kernel /= booted_kernel) $ withColor "ffff00" "Booted kernel stale"
        , \var -> do
            module_state <- newTVarIO ("", "", "")
            dirty_var <- newTVarIO (False, False)
            host_name <- ByteString.strip <$> readFileBS "/etc/hostname"
            ( simpleModule (60 * oneSecond) $ do
                current_commit <- readFileBS "/home/maralorn/git/config/.git/refs/heads/main"
                current_system <- readlink "/run/current-system" |> captureTrim
                current_modes <- readlink "/home/maralorn/.volatile/modes" |> captureTrim
                some_change <- atomically $ STM.stateTVar module_state \(previous_commit, previous_system, previous_modes) ->
                  (previous_commit /= current_commit || previous_system /= current_system || previous_modes /= current_modes, (current_commit, current_system, current_modes))
                when some_change do
                  next_system <- nix "eval" "--raw" ([i|/home/maralorn/git/config\#nixosConfigurations.#{host_name}.config.system.build.toplevel|] :: String) |> captureTrim
                  next_modes <- nix "eval" "--raw" ([i|/home/maralorn/git/config\#homeModes.#{host_name}|] :: String) |> captureTrim
                  atomically $ writeTVar dirty_var (current_system /= next_system, current_modes /= next_modes)
                (system_dirty, modes_dirty) <- readTVarIO dirty_var
                when' (system_dirty || modes_dirty) $ withColor "ffff00" [i|Current #{case (system_dirty,modes_dirty) of (True, True) -> "home and system"; (True, _) -> "system"; _ -> "home"} stale|]
              )
              var
        , \var ->
            onUpdate mode_var $ updateVarIfChanged var . runIdentity . withColor "7287fd" . show
        ]
  foldConcurrently_
    [ void $ simpleModule oneSecond getMode mode_var
    , runModules modules
    ]
