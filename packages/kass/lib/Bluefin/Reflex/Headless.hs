{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore #-}
module Bluefin.Reflex.Headless (runReflexHeadless) where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Internal qualified as Internal
import Bluefin.Reflex
import Bluefin.State
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad.Ref (readRef)
import Data.Dependent.Sum (DSum (..), (==>))
import Data.Traversable (for)
import Reflex hiding (Reflex, runRequesterT)
import Reflex qualified
import Reflex.Host.Class
import Reflex.Spider.Internal (HasSpiderTimeline)
import Relude hiding (Handle, runState)
import Relude.Monad.Reexport qualified as MTL

-- | Reflex Handler
runReflexHeadless
  :: (forall t er. Reflex.Reflex t => Reflex Headless t er -> Eff (er :& es) (Event t a))
  -> Eff es a
runReflexHeadless network =
  Internal.UnsafeMkEff $ runHeadlessApp $ inHeadlessApp network

inHeadlessApp
  :: HasSpiderTimeline x
  => (forall er. Reflex Headless (SpiderTimeline x) er -> Eff (er :& es) b)
  -> TriggerEventT
      (SpiderTimeline x)
      ( PostBuildT
          (SpiderTimeline x)
          (PerformEventT (SpiderTimeline x) (SpiderHost x))
      )
      b
inHeadlessApp network = do
  triggerChan <- TriggerEventT ask
  postBuild <- getPostBuild
  initialRequesterState <- lift . lift . PerformEventT . RequesterT $ MTL.get
  requesterSelector <- lift . lift . PerformEventT . RequesterT . lift $ ask
  (ret, finalState) <- liftIO . Internal.unsafeUnEff $ runState initialRequesterState \requesterStateHandle ->
    let
      spiderData =
        MkSpiderData
          { triggerChan
          , postBuild
          , requesterStateHandle
          , requesterSelector
          }
      r =
        ReflexHandle
          { spiderData
          , payload = Headless
          , runWithReplaceImpl = \(ReflexAction initial) ev ->
              reflexRunSpiderData spiderData $
                runWithReplace
                  (liftIO . Internal.unsafeUnEff $ initial r)
                  (ev <&> \(ReflexAction a) -> inHeadlessApp a)
          }
     in
      network r
  lift . lift . PerformEventT . RequesterT $ MTL.put finalState
  pure ret

data Headless t es = Headless

instance Handle (Headless t) where
  mapHandle = const Headless

--- Everything below this point is basically copied from Reflex.Host.Headless

{- |
This is a copy of Reflex.Host.Headless.runHeadlessApp just with a non-type-class type signature

Run a headless FRP network. Inside the action, you will most probably use
the capabilities provided by the 'TriggerEvent' and 'PerformEvent' type
classes to interface the FRP network with the outside world. Useful for
testing. Each headless network runs on its own spider timeline.
-}
runHeadlessApp
  :: ( forall x
        . HasSpiderTimeline x
       => TriggerEventT
            (SpiderTimeline x)
            ( PostBuildT
                (SpiderTimeline x)
                (PerformEventT (SpiderTimeline x) (SpiderHost x))
            )
            (Event (SpiderTimeline x) a)
     )
  -- ^ The action to be run in the headless FRP network. The FRP network is
  -- closed at the first occurrence of the resulting 'Event'.
  -> IO a
runHeadlessApp guest =
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline.
  withSpiderTimeline $ runSpiderHostForTimeline $ do
    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan
    -- Run the "guest" application, providing the appropriate context. We'll
    -- pure the result of the action, and a 'FireCommand' that will be used to
    -- trigger events.
    (result, fc@(FireCommand fire)) <- do
      hostPerformEventT $
        flip runPostBuildT postBuild $
          flip runTriggerEventT events $
            guest -- Allows the guest app to run
            -- 'performEvent', so that actions
            -- (e.g., IO actions) can be run when
            -- 'Event's fire.
            -- Allows the guest app to access to
            -- a "post-build" 'Event'
            -- Allows the guest app to create new
            -- events and triggers and write
            -- those triggers to a channel from
            -- which they will be read and
            -- processed.

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent result

    -- When there is a subscriber to the post-build event, fire the event.
    initialShutdownEventFirings :: Maybe [Maybe a] <- for mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ sequence =<< readEvent shutdown
    let shutdownImmediately = case initialShutdownEventFirings of
          -- We didn't even fire postBuild because it wasn't subscribed
          Nothing -> Nothing
          -- Take the first Just, if there is one. Ideally, we should cut off
          -- the event loop as soon as the firing happens, but Performable
          -- doesn't currently give us an easy way to do that
          Just firings -> asum firings

    case shutdownImmediately of
      Just exitResult -> pure exitResult
      -- The main application loop. We wait for new events and fire those that
      -- have subscribers. If we detect a shutdown request, the application
      -- terminates.
      Nothing -> fix $ \loop -> do
        -- Read the next event (blocking).
        ers <- liftIO $ readChan events
        shutdownEventFirings :: [Maybe a] <- do
          -- Fire events that have subscribers.
          fireEventTriggerRefs fc ers $
            -- Check if the shutdown 'Event' is firing.
            sequence
              =<< readEvent shutdown
        let
          -- If the shutdown event fires multiple times, take the first one.
          -- Ideally, we should cut off the event loop as soon as this fires,
          -- but Performable doesn't currently give us an easy way to do that.
          shutdownNow = asum shutdownEventFirings
        case shutdownNow of
          Just exitResult -> pure exitResult
          Nothing -> loop
 where
  -- Use the given 'FireCommand' to fire events that have subscribers
  -- and call the callback for the 'TriggerInvocation' of each.
  fireEventTriggerRefs
    :: forall b m t
     . MonadIO m
    => FireCommand t m
    -> [DSum (EventTriggerRef t) TriggerInvocation]
    -> ReadPhase m b
    -> m [b]
  fireEventTriggerRefs (FireCommand fire) ers rcb = do
    mes <- liftIO $
      for ers $
        \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          pure $ fmap (==> a) me
    a <- fire (catMaybes mes) rcb
    liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
    pure a
