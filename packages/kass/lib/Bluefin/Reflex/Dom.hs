module Bluefin.Reflex.Dom (Dom, runReflexDomGUI, runReflexDomServer, dom, BFWidget) where

import Bluefin.Internal (Eff (UnsafeMkEff))
import Bluefin.Reflex
import Control.Concurrent (Chan)
import Data.Dependent.Sum (DSum)
import GHC.Base qualified as GHC
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Warp qualified as Warp
import Language.Javascript.JSaddle.WebKitGTK qualified as GTK
import Maralude
import Reflex hiding (Reflex)
import Reflex qualified
import Reflex.Dom.Core hiding (Reflex)
import Reflex.Requester.Base.Internal (RequesterState)
import Reflex.Spider.Internal (SpiderHostFrame)
import Relude.Monad.Reexport qualified as MTL
import Unsafe.Coerce (unsafeCoerce)

data Dom t (es :: Effects) where
  DomHandle
    :: { env :: HydrationDomBuilderEnv DomTimeline (DomCoreWidget x)
       , jsContext :: JSContextSingleton x
       , jsmRequesterState :: State (RequesterState DomTimeline JSM) es
       , requesterSelector :: EventSelectorInt DomTimeline GHC.Any
       }
    -> Dom (SpiderTimeline Global) es

type BFWidget es = forall t e. Reflex.Reflex t => Reflex Dom t e -> Eff (e :& es) ()

runReflexDom
  :: ei :> es
  => (JSM () -> IO ())
  -> BFWidget es
  -> BFWidget es
  -> IOE ei
  -> Eff es ()
runReflexDom = \runner html_head body -> withEffToIO \runInIO ->
  runner
    $ mainWidgetWithHead
      (mkWidget runInIO html_head)
      (mkWidget runInIO body)

mkWidget
  :: (forall r. (forall e1. IOE e1 -> Eff (e1 :& es) r) -> IO r)
  -> BFWidget es
  -> (forall x. Widget x ())
mkWidget = \runInIO act -> do
  env <- unsafeHydrationDomBuilderT ask
  jsmRequesterPre <- unsafeHydrationDomBuilderT . lift . RequesterT $ MTL.get
  jsContext <- unsafeHydrationDomBuilderT . lift . RequesterT . lift . lift . lift . lift . WithJSContextSingleton $ ask
  requesterPre <-
    unsafeHydrationDomBuilderT . lift . RequesterT . lift . lift . lift . lift . lift . PerformEventT . RequesterT $ MTL.get
  reflexRequesterSelector <-
    unsafeHydrationDomBuilderT
      . lift
      . RequesterT
      . lift
      . lift
      . lift
      . lift
      . lift
      . PerformEventT
      . RequesterT
      . lift
      $ ask
  domRequesterSelector <- unsafeHydrationDomBuilderT . lift . RequesterT . lift $ ask
  triggerChan <- unsafeHydrationDomBuilderT . lift . RequesterT . lift . lift . TriggerEventT $ ask
  postBuild <- getPostBuild
  (((), jsmRequesterPost), requesterPost) <- liftIO $ runInIO $ \_ -> do
    useImpl $ runState requesterPre \requesterStateHandle -> runState jsmRequesterPre \jsmRequesterState ->
      assoc1Eff
        $ act
          ReflexHandle
            { spiderData =
                MkSpiderData
                  { postBuild
                  , requesterStateHandle = mapHandle requesterStateHandle
                  , triggerChan
                  , requesterSelector = reflexRequesterSelector
                  }
            , payload =
                DomHandle
                  { env
                  , jsmRequesterState = mapHandle jsmRequesterState
                  , jsContext
                  , requesterSelector = domRequesterSelector
                  }
            , runWithReplaceImpl = _
            }
  unsafeHydrationDomBuilderT . lift . RequesterT $ MTL.put jsmRequesterPost
  unsafeHydrationDomBuilderT
    . lift
    . RequesterT
    . lift
    . lift
    . lift
    . lift
    . lift
    . PerformEventT
    . RequesterT
    $ MTL.put requesterPost

runReflexDomGUI
  :: ei :> es
  => BFWidget es
  -> BFWidget es
  -> IOE ei
  -> Eff es ()
runReflexDomGUI = runReflexDom GTK.run

runReflexDomServer
  :: ei :> es
  => Int
  -> BFWidget es
  -> BFWidget es
  -> IOE ei
  -> Eff es ()
runReflexDomServer = \port -> runReflexDom \app -> do
  liftIO $ say "Starting Server ..."
  Warp.run port app

unsafeUnHydrationDomBuilderT
  :: HydrationDomBuilderT s t m a -> ReaderT (HydrationDomBuilderEnv t m) (RequesterT t JSM Identity (TriggerEventT t m)) a
unsafeUnHydrationDomBuilderT = unsafeCoerce

unsafeHydrationDomBuilderT
  :: ReaderT (HydrationDomBuilderEnv t m) (RequesterT t JSM Identity (TriggerEventT t m)) a -> HydrationDomBuilderT s t m a
unsafeHydrationDomBuilderT = unsafeCoerce

dom
  :: forall t e es r. e :> es => Reflex Dom t e -> (forall m. (DomBuilder t m, MonadReflex t m) => m r) -> Eff es r
dom ReflexHandle{spiderData, payload = d@(DomHandle env con _ _)} act = do
  modifyM @es d.jsmRequesterState \jsmPreState ->
    modifyM @es (spiderData.requesterStateHandle) \preState ->
      UnsafeMkEff
        $ runWidget
          act
          spiderData.requesterSelector
          d.requesterSelector
          con
          spiderData.postBuild
          spiderData.triggerChan
          env
          jsmPreState
          preState

modifyM :: forall ein st e s r. (st :> e, ein :> e) => State s st -> (s -> Eff ein (r, s)) -> Eff e r
modifyM st act = do
  preState <- get st
  (ret, postState) <- useImpl $ act preState
  put st postState
  pure ret

runWidget
  :: Widget x r
  -> EventSelectorInt DomTimeline GHC.Any
  -> EventSelectorInt DomTimeline GHC.Any
  -> JSContextSingleton x
  -> Event DomTimeline ()
  -> Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]
  -> HydrationDomBuilderEnv
      DomTimeline
      ( PostBuildT
          DomTimeline
          (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))
      )
  -> RequesterState DomTimeline JSM
  -> RequesterState DomTimeline (SpiderHostFrame Global)
  -> IO ((r, RequesterState DomTimeline JSM), (RequesterState DomTimeline (SpiderHostFrame Global)))
runWidget act reflexEventSelector domEventSelector con postBuild triggerChan domBuilderEnv jsmPreState preState = do
  runPerformEventT reflexEventSelector preState
    . runJSContext con
    . flip runPostBuildT postBuild
    . flip runTriggerEventT triggerChan
    . Bluefin.Reflex.runRequesterT domEventSelector jsmPreState
    . flip runReaderT domBuilderEnv
    . unsafeUnHydrationDomBuilderT
    $ act

runJSContext :: JSContextSingleton x -> WithJSContextSingleton x m a -> m a
runJSContext context = flip runReaderT context . unWithJSContextSingleton
