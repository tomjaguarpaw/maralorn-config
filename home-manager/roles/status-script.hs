{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch, onException)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Relude
import Say (sayErr)
import Shh (ExecReference (Absolute), Proc, captureTrim, exe, ignoreFailure, load, (|>))
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

data Var a = MkVar
  { value :: TVar a
  , update :: TMVar ()
  }

newVar :: a -> IO (Var a)
newVar initial_value = atomically $ MkVar <$> newTVar initial_value <*> newEmptyTMVar

newMaybeVar :: IO (Var (Maybe a))
newMaybeVar = atomically $ MkVar <$> newTVar Nothing <*> newEmptyTMVar

type Vars = [Var (Maybe Text)]
type Module a = Var a -> IO Void

repeatM :: IO a -> IO Void
repeatM action = fix (action >>)

writeVars :: Vars -> IO Void
writeVars vars = repeatM do
  outputs <- atomically $ do
    updates <- forM vars $ \MkVar{update} -> tryTakeTMVar update
    STM.check $ any isJust updates
    forM vars \MkVar{value} -> readTVar value
  writeFileText "/run/user/1000/status-bar" $
    Text.replace "&" "&amp;" $
      Text.unwords $
        ("<executor.markup.true>" :) $
          catMaybes outputs

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

updateVarIfChanged :: Eq a => Var a -> a -> STM ()
updateVarIfChanged MkVar{value, update} new_value = do
  old_value <- readTVar value
  unless (old_value == new_value) do
    writeTVar value new_value
    putTMVar update ()

simpleModule :: Eq a => Int -> IO a -> Module a
simpleModule delay action var = repeatM do
  atomically . updateVarIfChanged var =<< action
  Concurrent.threadDelay delay

withColor :: Monad m => Text -> Text -> m (Maybe Text)
withColor color content = pure $ Just [i|<span foreground='\##{color}'>#{content}</span>|]

when' :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
when' cond result = if cond then result else pure Nothing

main :: IO ()
main = do
  mode_var <- newVar Unrestricted
  let read_mode = readTVarIO (value mode_var)
      modules =
        [ simpleModule (5 * oneSecond) $ do
            appointments <- lines . decodeUtf8 <$> tryCmd (khal ["list", "-a", "Standard", "-a", "Planung", "-a", "Uni", "-a", "Maltaire", "now", "2h", "-df", ""])
            when' (not $ null appointments) $
              withColor "8839ef" (Text.intercalate "; " appointments)
        , simpleModule oneSecond $
            Just
              . Text.replace "Stopped -" "⏹"
              . Text.replace "Playing -" "▶"
              . Text.replace "Paused -" "⏸"
              . Text.intercalate " - "
              . fmap decodeUtf8
              . filter (/= "")
              <$> mapM
                tryCmd
                [ playerctl "status"
                , playerctl "metadata" "title"
                , playerctl "metadata" "album"
                , playerctl "metadata" "artist"
                ]
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
            current_kernel <- readlink "/run/current-system/kernel"
            booted_kernel <- readlink "/run/booted-system/kernel"
            when' (current_kernel /= booted_kernel) $ withColor "ffff00" "Booted kernel stale"
        , \var -> do
            module_state <- newTVarIO ("", "")
            dirty_var <- newTVarIO False
            host_name <- ByteString.strip <$> readFileBS "/etc/hostname"
            ( simpleModule (60 * oneSecond) $ do
                current_commit <- readFileBS "/home/maralorn/git/config/.git/refs/heads/main"
                current_system <- readlink "/run/current-system" |> captureTrim
                some_change <- atomically $ STM.stateTVar module_state \(previous_commit, previous_system) ->
                  (previous_commit /= current_commit || previous_system /= current_system, (current_commit, current_system))
                when some_change do
                  next_system <- nix "eval" "--raw" ([i|/disk/persist/maralorn/git/config\#nixosConfigurations.#{host_name}.config.system.build.toplevel|] :: String) |> captureTrim
                  atomically $ writeTVar dirty_var (current_system /= next_system)
                is_dirty <- readTVarIO dirty_var
                when' is_dirty $ withColor "ffff00" "Current system stale"
              )
              var
        , \var -> do
            let show_mode = do
                  mode <- read_mode
                  withColor "7287fd" (show mode)
            show_mode
            ( simpleModule 1 $ do
                atomically $ takeTMVar (update mode_var)
                show_mode
              )
              var
        ]
  foldConcurrently_
    [ void $ simpleModule oneSecond getMode mode_var
    , runModules modules
    ]
