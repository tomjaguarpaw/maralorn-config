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

watchDir :: (R.MonadHeadlessApp t m) => Env -> FilePath -> Bool -> Notify.ActionPredicate -> m (R.Event t Notify.Event)
watchDir env path recursive predicate = do
  let watch = if recursive then Notify.watchTree else Notify.watchDir
  R.newEventWithLazyTriggerWithOnComplete \callback -> do
    finish_callback <- newEmptyTMVarIO
    env.fork [i|Activating watches for dir #{path}|] do
      cb <- watch env.watch_manager path predicate (`callback` pass)
      atomically $ putTMVar finish_callback cb
    pure $ env.fork [i|Deactivating watches for dir #{path}|] $ join $ atomically $ takeTMVar finish_callback

watchFile :: (R.MonadHeadlessApp t m) => Env -> FilePath -> FilePath -> m (R.Event t ())
watchFile env dir file = do
  start <- R.getPostBuild
  event <-
    watchDir
      env
      dir
      False
      (Notify.eventPath % List.isSuffixOf file)
      <&> void
  pure $ R.leftmost [start, event]

watchFileContents :: (R.MonadHeadlessApp t m) => Env -> FilePath -> FilePath -> m (R.Dynamic t (Maybe Text))
watchFileContents env dir file = do
  event_event <- watchFile env dir file
  let read = do
        sayErr [i|Reading #{dir </> file}|]
        content <-
          readFileBS (dir </> file)
            & Exception.try @Exception.IOException
            <&> either
              (const Nothing)
              ( ByteStringChar.strip
                  % decodeUtf8Strict @Text
                  % hush
              )
        sayErr [i|Read: #{content}|]
        unless (isJust content) $ sayErr [i|Failed to read #{dir </> file}|]
        pure content
  content_event <- ReflexUtil.performEventThreaded env event_event (const read)
  start_val <- liftIO read
  stored_event <- R.holdDyn start_val content_event
  R.holdUniqDyn stored_event
