module StatusScript.ReflexUtil (performEventThreaded) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R

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
