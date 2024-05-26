{-# LANGUAGE UnboxedTuples #-}
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
  , withReflex
  , reflexRunSpiderData
  , ReflexAction (..)
  , runWithReplaceEff
  , dynEffEv
  , dynEff
  , dynToEv
  )
where

import Bluefin.Internal (Eff (UnsafeMkEff), unsafeUnEff, weakenEff)
import Bluefin.Internal qualified as BF
import Control.Concurrent (Chan)
import Data.Dependent.Sum (DSum)
import GHC.Base qualified as GHC
import Maralude
import Reflex hiding (Reflex, runRequesterT)
import Reflex qualified
import Reflex.Requester.Base.Internal (RequesterState)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderHostFrame, runSpiderHostFrame, unEventM)

deriving newtype instance MonadFix (Eff es)

-- | Reflex Effect Handle
data Reflex a t (es :: Effects) where
  ReflexHandle
    :: { spiderData :: SpiderData t es
       , payload :: a t es
       , runWithReplaceImpl
          :: forall e r b
           . ReflexAction a t e r
          -> (Event t (ReflexAction a t e b))
          -> Eff (es :& e) (r, Event t b)
       }
    -> Reflex a t es

newtype ReflexAction h t e b = ReflexAction (forall ei. Reflex h t ei -> Eff (ei :& e) b)

runWithReplaceEff
  :: e :> es
  => Reflex h t e
  -> ReflexAction h t es r
  -> (Event t (ReflexAction h t es b))
  -> Eff es (r, Event t b)
runWithReplaceEff = \ReflexHandle{runWithReplaceImpl} -> fmap inContext' <$> runWithReplaceImpl

dynEff :: e :> es => Reflex h t e -> Dynamic t (ReflexAction h t es b) -> Eff es (Event t b)
dynEff = \r dyn' -> do
  in_ev <- dynToEv r dyn'
  (_, ev) <- runWithReplaceEff r (ReflexAction (const pass)) in_ev
  pure ev

dynToEv :: e :> es => Reflex h t e -> Dynamic t a -> Eff es (Event t a)
dynToEv = \r dyn' -> do
  pb <- reflex r getPostBuild
  pure (withReflex r (leftmost [updated dyn', current dyn' <@ pb]))

dynEffEv :: e :> es => Reflex h t e -> Dynamic t (ReflexAction h t es (Event t b)) -> Eff es (Event t b)
dynEffEv = \r dyn' -> do
  ev <- dynEff r dyn'
  reflex r $ switchHold never ev

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
  mapHandle = \ReflexHandle{spiderData, payload, runWithReplaceImpl} ->
    ReflexHandle
      { spiderData = mapHandle spiderData
      , payload = mapHandle payload
      , runWithReplaceImpl = \initial ev -> weakenEff (BF.bimap BF.has (BF.eq (# #))) $ runWithReplaceImpl initial ev
      }

withReflex :: Reflex h t es -> (Reflex.Reflex t => a) -> a
withReflex = \ReflexHandle{spiderData = MkSpiderData{}} x -> x

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
performEffEvent r ev = reflexRunSpiderData r.spiderData $ performEvent (liftIO . unsafeUnEff <$> ev)

reflex
  :: forall a t e es r
   . e :> es
  => Reflex a t e
  -> (forall m. MonadReflex t m => m r)
  -> Eff es r
reflex r a = reflexRunSpiderData r.spiderData a

reflexIO
  :: (er :> es, eio :> es)
  => IOE eio
  -> Reflex a t er
  -> (forall m. MonadReflexIO t m => m r)
  -> Eff es r
reflexIO _ r a = reflexRunSpiderData r.spiderData a

-- | Reflex Actions
reflexRunSpiderData
  :: e :> es
  => SpiderData t e
  -> ( forall x
        . (SpiderTimeline x ~ t, HasSpiderTimeline x)
       => TriggerEventT
            (SpiderTimeline x)
            ( PostBuildT
                (SpiderTimeline x)
                (PerformEventT (SpiderTimeline x) (SpiderHost x))
            )
            r
     )
  -> Eff es r
reflexRunSpiderData d@MkSpiderData{} act = do
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
