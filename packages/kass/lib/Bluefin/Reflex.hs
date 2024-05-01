{-# OPTIONS_GHC -Wno-orphans #-}

module Bluefin.Reflex
  ( Reflex (..)
  , SpiderData (..)
  , reflex
  , reflexIO
  , MonadReflex
  , MonadReflexIO
  , performEffEvent
  , runRequesterT
  , runPerformEventT
  )
where

import Bluefin.Internal (Eff (UnsafeMkEff), unsafeUnEff)
import Control.Concurrent (Chan)
import Data.Dependent.Sum (DSum)
import GHC.Base qualified as GHC
import Maralude
import Reflex hiding (Reflex, runRequesterT)
import Reflex.Requester.Base.Internal (RequesterState)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderHostFrame, runSpiderHostFrame, unEventM)

deriving newtype instance MonadFix (Eff es)

-- | Reflex Effect Handle
data Reflex a t (es :: Effects) where
  ReflexHandle
    :: { spiderData :: SpiderData t es
       , payload :: a t es
       }
    -> Reflex a t es

data SpiderData t es where
  MkSpiderData
    :: HasSpiderTimeline x
    => { triggerChan :: Chan [DSum (EventTriggerRef (SpiderTimeline x)) TriggerInvocation]
       , postBuild :: Event (SpiderTimeline x) ()
       , requesterStateHandle :: State (RequesterState (SpiderTimeline x) (SpiderHostFrame x)) es
       , requesterSelector :: EventSelectorInt (SpiderTimeline x) GHC.Any
       }
    -> SpiderData (SpiderTimeline x) es

instance Handle (SpiderData t) where
  mapHandle = \d@MkSpiderData{} -> d{requesterStateHandle = mapHandle d.requesterStateHandle}

instance Handle (a t) => Handle (Reflex a t) where
  mapHandle = \ReflexHandle{spiderData, payload} ->
    ReflexHandle
      { spiderData = mapHandle spiderData
      , payload = mapHandle payload
      }

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

performEffEvent :: er :> es => Reflex s t er -> Event t (Eff es a) -> Eff es (Event t a)
performEffEvent r ev = reflexUnsafe r $ performEvent (liftIO . unsafeUnEff <$> ev)

reflex
  :: forall a t e es r
   . e :> es
  => Reflex a t e
  -> (forall m. MonadReflex t m => m r)
  -> Eff es r
reflex r a = reflexUnsafe r a

reflexIO
  :: (er :> es, eio :> es)
  => IOE eio
  -> Reflex a t er
  -> (forall m. MonadReflexIO t m => m r)
  -> Eff es r
reflexIO _ r a = reflexUnsafe r a

-- | Reflex Actions
reflexUnsafe
  :: e :> es
  => Reflex a t e
  -> (forall m. MonadReflexIO t m => m r)
  -> Eff es r
reflexUnsafe ReflexHandle{spiderData = d@MkSpiderData{}} act = do
  preState <- get d.requesterStateHandle
  (ret, postState) <-
    UnsafeMkEff
      . runPerformEventT d.requesterSelector preState
      . flip runPostBuildT d.postBuild
      . flip runTriggerEventT d.triggerChan
      $ act
  put d.requesterStateHandle postState
  pure ret

runRequesterT
  :: EventSelectorInt t GHC.Any
  -> RequesterState t request
  -> RequesterT t request response m a
  -> m (a, RequesterState t request)
runRequesterT requesterSelector preState =
  flip runReaderT requesterSelector
    . flip runStateT preState
    . unRequesterT

runPerformEventT
  :: EventSelectorInt (SpiderTimeline x) GHC.Any
  -> RequesterState (SpiderTimeline x) (SpiderHostFrame x)
  -> PerformEventT (SpiderTimeline x) (SpiderHost w) a
  -> IO (a, RequesterState (SpiderTimeline x) (SpiderHostFrame x))
runPerformEventT requesterSelector preState =
  unEventM
    . runSpiderHostFrame
    . runRequesterT requesterSelector preState
    . unPerformEventT
