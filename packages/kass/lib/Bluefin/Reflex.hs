module Bluefin.Reflex (runReflex, reflex, reflexIO, MonadReflex, main) where

import Bluefin.Internal (Eff (UnsafeMkEff), unsafeUnEff)
import Bluefin.Reflex.Headless.Internal
import Control.Concurrent (Chan)
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum (..))
import GHC.Base qualified as GHC
import Maralude
import Reflex
  ( Adjustable
  , Event
  , EventSelectorInt
  , EventTriggerRef
  , MonadHold
  , MonadSample
  , NotReady
  , PerformEvent
  , PerformEventT (PerformEventT)
  , Performable
  , PostBuild
  , Reflex
  , RequesterT (RequesterT)
  , SpiderHost
  , SpiderTimeline
  , TriggerEvent
  , TriggerEventT (TriggerEventT)
  , TriggerInvocation
  , constDyn
  , delay
  , foldDyn
  , getPostBuild
  , performEvent
  , runPostBuildT
  , runTriggerEventT
  , unPerformEventT
  , unRequesterT
  , updated
  )
import Reflex.Requester.Base.Internal (RequesterState)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderHostFrame, runSpiderHostFrame, unEventM)
import Relude.Monad qualified as MTL

-- | Reflex Effect Handle
data ReflexE t (es :: Effects) where
  MkReflex
    :: HasSpiderTimeline x
    => { triggerChan :: Chan [DSum (EventTriggerRef (SpiderTimeline x)) TriggerInvocation]
       , postBuild :: Event (SpiderTimeline x) ()
       , requesterStateHandle :: State (RequesterState (SpiderTimeline x) (SpiderHostFrame x)) es
       , requesterSelector :: EventSelectorInt (SpiderTimeline x) GHC.Any
       }
    -> ReflexE (SpiderTimeline x) es

type MonadReflexIO t m =
  ( Adjustable t m
  , -- , MonadCatch m
    MonadFix (Performable m)
  , MonadFix m
  , -- MonadHold t (Performable m)
    MonadHold t m
  , -- , MonadIO (HostFrame t)
    MonadIO (Performable m)
  , MonadIO m
  , -- MonadMask m
    -- , MonadRef (HostFrame t)
    --  MonadSample t (Performable m)
    MonadSample t m
  , TriggerEvent t m
  , -- , MonadThrow m
    NotReady t m
  , PerformEvent t m
  , PostBuild t m
  -- , PrimMonad (HostFrame t)
  -- Causes issues: , Ref (HostFrame t) ~ IORef
  -- , Ref m ~ IORef
  -- , ReflexHost t
  )

type MonadReflex t m =
  ( Adjustable t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t m
  , TriggerEvent t m
  , NotReady t m
  , PerformEvent t m
  , PostBuild t m
  )

-- | Reflex Handler
runReflex
  :: (forall t er. Reflex t => ReflexE t er -> Eff (er :& es) (Event t a))
  -> Eff es a
runReflex act =
  UnsafeMkEff $ runHeadlessApp do
    triggerChan <- TriggerEventT ask
    postBuild <- getPostBuild
    initialRequesterState <- lift . lift . PerformEventT . RequesterT $ MTL.get
    requesterSelector <- lift . lift . PerformEventT . RequesterT . lift $ ask
    (ret, finalState) <- liftIO . unsafeUnEff $ runState initialRequesterState $ \requesterStateHandle ->
      act
        $ MkReflex
          { triggerChan
          , postBuild
          , requesterStateHandle
          , requesterSelector
          }
    lift . lift . PerformEventT . RequesterT $ MTL.put finalState
    pure ret

performEffEvent :: er :> es => ReflexE t er -> Event t (Eff es a) -> Eff es (Event t a)
performEffEvent r ev = reflexUnsafe r $ performEvent (liftIO . unsafeUnEff <$> ev)

reflex
  :: e :> es
  => ReflexE t e
  -> (forall m. MonadReflex t m => m r)
  -> Eff es r
reflex r a = reflexUnsafe r a

reflexIO
  :: (er :> es, eio :> es)
  => IOE eio
  -> ReflexE t er
  -> (forall m. MonadReflexIO t m => m r)
  -> Eff es r
reflexIO _ r a = reflexUnsafe r a

-- | Reflex Actions
reflexUnsafe
  :: e :> es
  => ReflexE t e
  -> (forall m. MonadReflexIO t m => m r)
  -> Eff es r
reflexUnsafe r@(MkReflex @x _x _ _ _) act = do
  preState <- get r.requesterStateHandle
  (ret, postState) <-
    UnsafeMkEff
      . unEventM
      . runSpiderHostFrame
      . flip runReaderT r.requesterSelector
      . flip runStateT preState
      . unRequesterT
      . unPerformEventT @_ @(SpiderHost x)
      . flip runPostBuildT r.postBuild
      . flip runTriggerEventT r.triggerChan
      $ act
  put r.requesterStateHandle postState
  pure ret

-- Example App

main :: IO ()
main = runEff \io -> do
  result <- runReflex (app io)
  effIO io $ print result

app
  :: (e :> es, ei :> es, Reflex t)
  => IOE ei
  -> ReflexE t e
  -> Eff es (Event t Int)
app io r = do
  let myDyn = constDyn (5 :: Int)
  pb <- reflex r getPostBuild
  pbs <- reflex r $ foldDyn (\_ x -> x + 1) 0 pb
  foo <-
    performEffEvent r
      $ updated ((+) <$> myDyn <*> pbs)
      <&> \x -> do
        effIO io $ print x
        pure x
  reflexIO io r (delay 5 foo)
