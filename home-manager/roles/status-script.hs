{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Concurrent qualified
import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch, onException)
import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Relude
import Say (sayErr)
import Shh (ExecReference (Absolute), captureTrim, exe, ignoreFailure, load, (|>))
import System.Directory (listDirectory)

data Mode = Klausur | Orga | Communication | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

load Absolute ["git", "khal", "playerctl", "notmuch"]

modes = enumFrom Klausur

getMode = do
  name <- Text.strip <$> readFileText "/home/maralorn/.mode" `onException` sayErr "File /home/maralorn/.mode not found."
  maybe (sayErr [i|Unknown mode #{name}|] >> error [i|Unknown mode #{name}|]) pure $ find (\mode -> name == Text.toLower (show mode)) modes

isDirty gitDir = ((/= "") <$> (git "--no-optional-locks" "-C" gitDir "status" "--porcelain" |> captureTrim)) `catch` (\(_ :: SomeException) -> pure True)
isUnpushed gitDir = do
  revs <- tryCmd (git "--no-optional-locks" "-C" gitDir "branch" "-r" "--contains" "HEAD")
  pure $ LBS.null revs

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

main :: IO ()
main = do
  mode_var <- newVar Unrestricted
  let read_mode = readTVarIO (value mode_var)
      modules =
        [ simpleModule (5 * oneSecond) $ do
            appointments <- lines . decodeUtf8 <$> tryCmd (khal ["list", "-a", "Standard", "-a", "Planung", "-a", "Uni", "-a", "Maltaire", "now", "2h", "-df", ""])
            pure $
              if null appointments
                then Nothing
                else Just [i|<span foreground='\#8839ef'>#{Text.intercalate "; " appointments}</span>|]
        , simpleModule oneSecond $
            Just . Text.replace "Stopped -" "⏹" . Text.replace "Playing -" "▶" . Text.replace "Paused -" "⏸" . Text.intercalate " - " . fmap decodeUtf8 . filter (/= "") <$> mapM tryCmd [playerctl "status", playerctl "metadata" "title", playerctl "metadata" "album", playerctl "metadata" "artist"]
        , simpleModule oneSecond $ do
            mode <- read_mode
            unread <-
              if mode >= Orga
                then notmuch "count" "folder:hera/Inbox" "tag:unread" |> captureTrim
                else pure "0"
            pure $ memptyIfFalse (unread /= "0") (Just [i|<span foreground='\#d20f39'>Unread: #{unread}</span>|])
        , simpleModule oneSecond $ do
            mode <- read_mode
            inbox <-
              if mode == Leisure
                then notmuch "count" "folder:hera/Inbox" |> captureTrim
                else pure "0"
            pure $ memptyIfFalse (inbox /= "0") (Just [i|<span foreground='\#e53443'>Inbox: #{inbox}</span>|])
        , simpleModule oneSecond $ do
            mode <- read_mode
            codeMails <-
              if mode == Code
                then notmuch "count" "folder:hera/Code" |> captureTrim
                else pure "0"
            pure $ memptyIfFalse (codeMails /= "0") (Just [i|<span foreground='\#8839ef'>Code Mails: #{codeMails}</span>|])
        , simpleModule (5 * oneSecond) $ do
            mode <- read_mode
            codeUpdates <-
              if mode == Code
                then fromMaybe 0 . readMaybe . toString . Text.replace " unread articles" "" . decodeUtf8 <$> tryCmd (exe "software-updates" "-x" "print-unread")
                else pure 0
            pure $ memptyIfFalse (codeUpdates /= 0) (Just [i|<span foreground='\#179299'>Code Updates: #{codeUpdates}</span>|])
        , simpleModule (5 * oneSecond) $ do
            dirs <- listDirectory "/home/maralorn/git"
            dirty <- fmap toText <$> filterM (isDirty . ("/home/maralorn/git/" <>)) dirs
            pure $ memptyIfFalse (not (null dirty)) (Just [i|<span foreground='\#e64443'>Dirty: #{Text.intercalate " " dirty}</span>|])
        , simpleModule (5 * oneSecond) $ do
            dirs <- listDirectory "/home/maralorn/git"
            unpushed <- fmap toText <$> filterM (isUnpushed . ("/home/maralorn/git/" <>)) dirs
            pure $ memptyIfFalse (not (null unpushed)) (Just [i|<span foreground='\#fe640b'>Unpushed: #{Text.intercalate " " unpushed}</span>|])
        , simpleModule 1 $ do
            atomically $ takeTMVar (update mode_var)
            mode <- read_mode
            pure $ Just [i|<span foreground='\#7287fd'>#{show mode}</span>|]
        ]
  foldConcurrently_
    [ void $ simpleModule oneSecond getMode mode_var
    , runModules modules
    ]
