module StatusScript.ReflexUtil (performEventThreaded, concatEvents, processLines, tickEvent, taggedAndUpdated, updatedAndStart) where

import Control.Concurrent qualified as Conc
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.Env (Env (..))

tickEvent :: R.MonadHeadlessApp t m => Int -> m (R.Event t ())
tickEvent delay =
  R.tickLossyFromPostBuildTime (realToFrac delay)
    <&> void

taggedAndUpdated :: R.Reflex t => R.Dynamic t a -> R.Event t b -> R.Event t a
taggedAndUpdated = \dynamic event -> R.leftmost [R.updated dynamic, R.tag (R.current dynamic) event]

updatedAndStart :: R.MonadHeadlessApp t m => R.Dynamic t a -> m (R.Event t a)
updatedAndStart the_dyn = R.getPostBuild <&> taggedAndUpdated the_dyn

oneSecond :: Int
oneSecond = 1000000

concatEvents :: (R.MonadHeadlessApp t m, Monoid b, Eq b) => [R.Event t b] -> m (R.Event t b)
concatEvents =
  mapM (R.holdDyn mempty)
    %> mconcat
    >=> R.holdUniqDyn
    %> R.updated

processLines :: forall t m. R.MonadHeadlessApp t m => Env -> Shh.Proc () -> m (R.Event t ByteString)
processLines = \env command -> do
  (event, trigger) <- R.newTriggerEvent
  liftIO $ env.fork "Processing lines" $ forever do
    Conc.threadDelay oneSecond
    Shh.ignoreFailure
      ( command |> Shh.readInputLines (mapM_ (toStrict % trigger))
      )
    sayErr "A processLines command failed and will be restarted in a second."
  pure event

-- Call IO action in a separate thread. If multiple events fire never run two actions in parallel and if more than one action queues up, only run the latest.
performEventThreaded :: R.MonadHeadlessApp t m => Env -> R.Event t a -> (a -> IO b) -> m (R.Event t b)
performEventThreaded env event action = do
  runnerState <- liftIO newEmptyTMVarIO
  (out_event, callback) <- R.newTriggerEvent
  R.performEvent_ $ event <&> putTMVar runnerState % atomically % liftIO
  liftIO $ env.fork "event performing control thread" $ forever do
    input <- atomically $ do
      takeTMVar runnerState
    action input >>= callback
  pure out_event
