module StatusScript.ReflexUtil (performEventThreaded, concatEvents, oneSecond, processLines, reportMissing) where

import Control.Concurrent qualified as Conc
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified

data EventRunnerState a = Idle | Running | NextWaiting a

oneSecond :: Int
oneSecond = 1000000

concatEvents :: (R.MonadHeadlessApp t m, Monoid b, Eq b) => [R.Event t b] -> m (R.Event t b)
concatEvents =
  mapM (R.holdDyn mempty)
    %> mconcat
    >=> R.holdUniqDyn
    %> R.updated

processLines :: forall t m. R.MonadHeadlessApp t m => Shh.Proc () -> m (R.Event t ByteString)
processLines = \command -> do
  (event, trigger) <- R.newTriggerEvent
  void $ liftIO $ Conc.forkIO $ forever do
    Conc.threadDelay oneSecond
    Shh.ignoreFailure
      ( command |> Shh.readInputLines (mapM_ (toStrict % trigger))
      )
  pure event

reportMissing :: MonadIO m => IO [FilePath] -> m ()
reportMissing missing = whenJustM (nonEmpty <$> liftIO missing) \missing' -> sayErr [i|missing executables #{missing'}|]

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
