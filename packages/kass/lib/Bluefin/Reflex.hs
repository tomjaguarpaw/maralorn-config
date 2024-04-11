module Bluefin.Reflex (ReflexE (..), reflex, reflexIO, MonadReflex, performEffEvent) where

import Bluefin.Internal (Eff (UnsafeMkEff), unsafeUnEff)
import Control.Concurrent (Chan)
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum (..))
import GHC.Base qualified as GHC
import Maralude
import Reflex
import Reflex.Requester.Base.Internal (RequesterState)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderHostFrame, runSpiderHostFrame, unEventM)

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

-- Uncommented: Other available type classes which I don’t want to expose.
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
reflexUnsafe r@(MkReflex{}) act = do
  preState <- get r.requesterStateHandle
  (ret, postState) <-
    UnsafeMkEff
      . unEventM
      . runSpiderHostFrame
      . flip runReaderT r.requesterSelector
      . flip runStateT preState
      . unRequesterT
      . unPerformEventT @_ @(SpiderHost _)
      . flip runPostBuildT r.postBuild
      . flip runTriggerEventT r.triggerChan
      $ act
  put r.requesterStateHandle postState
  pure ret
