module StatusScript.FileWatch (watchDir, watchFile, watchFileContents) where

import Control.Concurrent.Async qualified as Async
import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as ByteStringChar
import Data.List qualified as List
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.ReflexUtil qualified as ReflexUtil
import System.FSNotify qualified as Notify
import System.FilePath ((</>))

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
    (Notify.eventPath % List.isSuffixOf file)
    <&> void
      % (<> start)

watchFileContents :: R.MonadHeadlessApp t m => Notify.WatchManager -> FilePath -> FilePath -> m (R.Event t Text)
watchFileContents watch_manager dir file = do
  event_event <- watchFile watch_manager dir file
  content_event <- ReflexUtil.performEventThreaded event_event \_ ->
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
