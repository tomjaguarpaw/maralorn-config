module StatusScript.FileWatch (watchDir, watchFile, watchFileContents) where

import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as ByteStringChar
import Data.List qualified as List
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import System.FSNotify qualified as Notify
import System.FilePath ((</>))

watchDir :: R.MonadHeadlessApp t m => Env -> FilePath -> Bool -> Notify.ActionPredicate -> m (R.Event t Notify.Event)
watchDir env path recursive predicate = do
  let watch = if recursive then Notify.watchTree else Notify.watchDir
  R.newEventWithLazyTriggerWithOnComplete \callback -> do
    finish_callback <- newEmptyTMVarIO
    env.fork [i|Activating watches for dir #{path}|] do
      cb <- Exception.handle
        ( \(e :: Exception.IOException) -> do
            sayErr [i|Failed to setup watch for #{path}: #{e}|]
            pure pass
        )
        do
          watch
            env.watch_manager
            path
            predicate
            (`callback` pass)
      atomically $ putTMVar finish_callback cb
    pure $ env.fork [i|Deactivating watches for dir #{path}|] $ Exception.handle
      (\(e :: Exception.IOException) -> sayErr [i|Failed to cleanup watches for #{path}: #{e}|])
      do join $ atomically $ takeTMVar finish_callback

watchFile :: R.MonadHeadlessApp t m => Env -> FilePath -> FilePath -> m (R.Event t ())
watchFile env dir file = do
  start <- R.getPostBuild
  event <-
    watchDir
      env
      dir
      False
      ( \event ->
          file `List.isSuffixOf` event.eventPath && case event of
            Notify.Added{} -> True
            Notify.Modified{} -> True
            Notify.Removed{} -> True
            _ -> False
      )
      <&> void
  R.throttle 0.2 (R.leftmost [start, event])

watchFileContents :: R.MonadHeadlessApp t m => Env -> FilePath -> FilePath -> m (R.Dynamic t (Maybe Text))
watchFileContents env dir file = do
  start_val <- liftIO read
  event_event <- watchFile env dir file
  content_event <- ReflexUtil.performEventThreaded env event_event (const read)
  stored_event <- R.holdDyn start_val content_event
  R.holdUniqDyn stored_event
 where
  read =
    readFileBS (dir </> file)
      & Exception.try @Exception.IOException
      <&> either
        (const Nothing)
        ( ByteStringChar.strip
            % decodeUtf8Strict @Text
            % hush
        )
