{-# LANGUAGE ImpredicativeTypes #-}

module Main (main, green) where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch, onException)
import Control.Exception qualified as Exception

import Data.ByteString.Char8 qualified as ByteStringChar
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC

import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Unique qualified as Unique
import Relude
import Say (say, sayErr)
import Shh (ExecReference (Absolute), Proc, captureTrim, exe, ignoreFailure, load, readInputLines, (&>), (|>))
import Shh qualified

import Control.Concurrent qualified as Conc
import Data.Set qualified as Set
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import System.Directory (listDirectory)
import System.Environment (getEnv)
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

data Mode = Klausur | Orga | Communication | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

load Absolute ["git", "khal", "playerctl", "notmuch", "readlink", "nix", "nix-diff", "jq"]
missingExecutables :: IO [FilePath]
modes :: [Mode]
modes = enumFrom Klausur

getMode :: FilePath -> IO Mode
getMode home = do
  let mode_file = home </> ".mode"
  name <- decodeUtf8 . ByteStringChar.strip <$> readFileBS mode_file `onException` sayErr [i|File #{mode_file} not found.|]
  maybe (sayErr [i|Unknown mode #{name}|] >> error [i|Unknown mode #{name}|]) pure $ find (\mode -> name == Text.toLower (show mode)) modes

-- modeFile = home </> ".mode"

-- toByteArray :: String -> Array Word8
-- toByteArray = Unsafe.unsafePerformIO . Stream.fold Array.write . Unicode.encodeUtf8 . Stream.fromList
--
-- modeStream :: Stream IO (Either Text Mode)
-- modeStream = do
--  FileSystemStream.watch (fromList [toByteArray home])
--    & ( Stream.mapMaybe \case
--          event | FileSystemStream.getRelPath event == toByteArray ".mode" -> Just ()
--          _ -> Nothing
--      )
--    & Stream.cons ()
--    & Stream.mapM \_ ->
--      do
--        name <- decodeUtf8 . ByteStringChar.strip <$> readFileBS modeFile `onException` sayErr [i|File #{modeFile} not found.|]
--        pure $ maybe (Left [i|Unknown mode #{name}|]) Right $ find (\mode -> name == Text.toLower (show mode)) modes
--
-- modeModuleStream :: Stream IO (Either Text Mode) -> Stream IO (Maybe Text)
-- modeModuleStream =
--  fmap $
--    Just . \case
--      Left err -> withColor' red err
--      Right mode -> withColor' blue (show mode)
--
-- hush = \case
--  Left _ -> Nothing
--  Right x -> Just x
--
-- setDefault :: Monad m => a -> Stream m a -> Stream m a
-- setDefault = \default_value -> Stream.scan (Fold.foldl' (const id) default_value)
--
-- withColor' :: Text -> Text -> Text
-- withColor' color content = [i|${color \##{color}}#{content}|]
--
-- defaultedModeStream :: Stream IO (Either Text Mode) -> Stream IO Mode
-- defaultedModeStream =
--  setDefault Orga
--    . Stream.catMaybes
--    . fmap hush

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

getUnique :: Var a -> STM Unique.Unique
getUnique = fmap snd . readTVar . value
getVal :: Var a -> STM a
getVal = fmap fst . readTVar . value

newVar :: a -> IO (Var a)
newVar initial_value = do
  unique <- Unique.newUnique
  MkVar <$> newTVarIO (initial_value, unique)

data Module a = OldModule ((a -> IO ()) -> IO Void) | Module (forall t m. R.MonadHeadlessApp t m => m (R.Event t a, IO ()))

separator :: Text
separator = "\n$color1$hr\n"

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

runModules :: R.MonadHeadlessApp t m => [Module (Maybe Text)] -> m ()
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

updateVarIfChanged :: Eq a => Var a -> a -> IO ()
updateVarIfChanged var@MkVar{value} new_value = do
  new_unique <- Unique.newUnique
  atomically do
    old_value <- getVal var
    unless (old_value == new_value) do
      writeTVar value (new_value, new_unique)

simpleModule :: Eq a => Int -> IO a -> Module a
simpleModule delay action = Module do
  tick <-
    R.tickLossyFromPostBuildTime (realToFrac delay / realToFrac oneSecond)
      <&> fmap (const $ liftIO action)
  event <- R.performEvent tick
  pure (event, pass)

withColor :: Monad m => Text -> Text -> m (Maybe Text)
withColor color content = pure $ Just [i|${color \##{color}}#{content}|]

when' :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
when' cond result = if cond then result else pure Nothing

playerCTLFormat :: String
playerCTLFormat = "@{{status}} {{title}} | {{album}} | {{artist}}"

playerModule :: FilePath -> Module (Maybe Text)
playerModule = \home -> Module do
  (event, trigger) <- R.newTriggerEvent
  let update_lines = mapM_ \update -> do
        mpdris_config <-
          [i|#{home}/.config/mpDris2/mpDris2.conf|]
            & readFileBS
              % Exception.try @Exception.IOException
        let host =
              mpdris_config
                & fromRight ""
                  % decodeUtf8
                  % lines
                  % mapMaybe (Text.stripPrefix "host = ")
                  % find (/= "::")
        update
          & decodeUtf8
            % Text.splitOn " | "
            % (maybeToList host <>)
            % filter (Text.null % not)
            % Text.intercalate "\n"
            % Text.replace "@Stopped" "⏹"
            % Text.replace "@Playing" "▶"
            % Text.replace "@Paused" "⏸"
            % withColor white
            % runIdentity
            % trigger
      listenToPlayer =
        forever $
          playerctl "metadata" "-F" "-f" playerCTLFormat
            |> Shh.readInputLines update_lines
  pure (event, listenToPlayer)

red :: Text
red = "F28FAD"
green :: String
green = "ABE9B3"
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
main = do
  Nothing <-
    missingExecutables
      <&> nonEmpty
  home <- getEnv "HOME"
  let git_dir = home </> "git"
      modes_dir = home </> ".volatile" </> "modes"
  mode_var <- newVar Unrestricted
  dirty_var <- newTVarIO []
  let read_mode = fst <$> readTVarIO (value mode_var)
      modules :: [Module (Maybe Text)] =
        [ simpleModule (5 * oneSecond) $ do
            appointments <- lines . decodeUtf8 <$> tryCmd (khal ["list", "-a", "Standard", "-a", "Planung", "-a", "Uni", "-a", "Maltaire", "now", "2h", "-df", ""])
            when' (not $ null appointments) $
              withColor magenta (Text.unlines appointments)
        , playerModule home
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
            mode <- read_mode
            when' (mode /= Klausur && not (null dirty)) $ withColor red [i|Dirty: #{Text.intercalate " " dirty}|]
        , simpleModule (5 * oneSecond) do
            dirs <- listDirectory git_dir
            unpushed <- fmap toText <$> filterM (isUnpushed . (git_dir </>)) dirs
            when' (not $ null unpushed) do withColor yellow [i|Unpushed: #{Text.intercalate " " unpushed}|]
        , simpleModule (5 * oneSecond) do
            let hosts = ["hera", "fluffy"]
            unreachable_hosts <- flip filterM hosts \host -> isLeft <$> (Shh.tryFailure do (exe "/run/wrappers/bin/ping" "-c" "1" (toString host)) &> Shh.devNull)
            when' ([] /= unreachable_hosts) do withColor red [i|No tunnel to #{Text.intercalate ", " unreachable_hosts}|]
        , simpleModule (5 * oneSecond) $ do
            current_kernel <- readlink "/run/current-system/kernel" |> captureTrim
            booted_kernel <- readlink "/run/booted-system/kernel" |> captureTrim
            mode <- read_mode
            when' (mode /= Klausur && current_kernel /= booted_kernel) $ withColor yellow "Booted kernel stale"
        , simpleModule (5 * oneSecond) $ do
            mode <- read_mode
            behind <-
              if mode /= Klausur
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
              mode <- read_mode
              var =<< if dirty || mode == Klausur then pure Nothing else scan
        , OldModule $ \var ->
            onUpdate mode_var $ var . runIdentity . withColor blue . show
        , simpleModule (1 * oneSecond) do
            now <- Time.getCurrentTime
            notifications <- processNotifications . fromRight "" <$> Exception.try @Exception.IOException (readFileBS [i|#{home}/.notifications/#{Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" now}.log|])
            when' (not $ Text.null notifications) $ withColor red (Text.take 24 (Text.drop ((`rem` 15) . round . Time.utctDayTime $ now) "NOTIFICATIONS! NOTIFICATIONS! NOTIFICATIONS!") <> "\n" <> notifications)
        ]
  R.runHeadlessApp do
    void $ liftIO $ Conc.forkIO $ forever do
      updateVarIfChanged mode_var =<< getMode home
      Concurrent.threadDelay oneSecond
    runModules modules
    pure R.never -- We have no exit condition.

processNotifications :: ByteString -> Text
processNotifications =
  Text.intercalate [i|\n$color1$hr${color \##{red}}\n|]
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
          last_message : rest_of_messages -> last_message <> "\n" <> line : rest_of_messages
      )
      []
    . lines
    . decodeUtf8
    . ByteStringChar.strip

notificationBlockList :: [Text]
notificationBlockList = ["Automatic suspend", "Auto suspend"]

diffIsSmall :: LBSC.ByteString -> LBSC.ByteString -> IO Bool
diffIsSmall = \pathA pathB -> (== "[]") <$> (nix_diff "--json" [pathA, pathB] |> jq ".inputsDiff.inputDerivationDiffs" |> captureTrim)

---- | Drain first stream exactly once, second stream as often as you want!
-- mirrorStream :: (MonadAsync m, MonadCatch m) => Stream m a -> m (Stream m a, Stream m a)
-- mirrorStream stream = do
--  emiters <- newIORef []
--  pure
--    ( stream
--        & Stream.finally
--          ( do
--              emiters' <- readIORef emiters
--              forM_ emiters' \emiter -> emiter Nothing
--          )
--        & Stream.mapM
--          ( \x -> do
--              emiters' <- readIORef emiters
--              forM_ emiters' \emiter -> emiter (Just x)
--              pure x
--          )
--    , Stream.fromCallback (\emit -> modifyIORef' emiters (emit :)) & Stream.takeWhile isJust & Stream.catMaybes
--    )
--
-- collectLatestJusts :: MonadAsync m => [Stream m (Maybe a)] -> Stream m [a]
-- collectLatestJusts =
--  fmap IntMap.elems
--    . Stream.scan (Fold.foldl' (&) IntMap.empty)
--    . Stream.parList id
--    . zipWith (\index stream -> (\new_value -> IntMap.alter (const new_value) index) <$> stream) [0 ..]
--
-- toStatusFile :: MonadIO m => Fold m [Text] ()
-- toStatusFile =
--  Fold.foldlM'
--    ( const \modules -> do
--        print modules
--        writeFileText "/run/user/1000/status-bar"
--          . Text.intercalate separator
--          . reverse
--          $ modules
--    )
--    pass
--
-- newmain :: IO ()
-- newmain = do
--  let modules =
--        [ modeModuleStream modeStream
--        ]
--  collectLatestJusts modules
--    --    & Stream.sampleIntervalEnd 0.05
--    & Stream.fold toStatusFile

{-
import Data.IntMap.Strict qualified as IntMap
import Effectful (Eff, IOE, (:>))
import Effectful qualified as Eff
import Relude
import Streamly.Internal.Data.Stream.Time qualified as Time
import Prelude ()
import Control.Monad.Catch (MonadCatch)

type SEff es a = Stream (Eff es) a

sourceEvent :: IOE :> es => SEff es Int
sourceEvent =
  Time.ticks 1
    & Stream.indexed
    & fmap fst

main :: IO ()
main =
  Eff.runEff $ Eff.withUnliftStrategy (Eff.ConcUnlift Eff.Persistent Eff.Unlimited) do
    (origStream, mirroredStream) <- mirrorStream sourceEvent
    let eventA = origStream <&> \x -> Just ("A: " <> show x)
        eventB = mirroredStream <&> \x -> Just ("B: " <> show x)
        eventC = mirroredStream <&> \x -> Just ("C: " <> show x)
    mkModules [eventA, eventB, eventC]
      & Stream.mapM (putTextLn . show)
      & Stream.fold Fold.drain
      -}
