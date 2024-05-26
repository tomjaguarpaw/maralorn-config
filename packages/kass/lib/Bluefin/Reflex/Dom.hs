module Bluefin.Reflex.Dom (Dom, runReflexDomServer, dom, BFWidget, withDom, elAttr, elClass, el, elClss) where

import Bluefin.Internal (Eff (UnsafeMkEff), unsafeUnEff)
import Bluefin.Reflex
import Control.Concurrent (Chan)
import Data.Dependent.Sum (DSum)
import GHC.Base qualified as GHC
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Warp qualified as Warp
import Maralude
import Reflex hiding (Reflex)
import Reflex qualified
import Reflex.Dom.Core hiding (Reflex, el, elAttr, elClass)
import Reflex.Dom.Core qualified as Dom
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

type BFWidget es a = forall t e. Reflex.Reflex t => Reflex Dom t e -> Eff (e :& es) a

type BFPartWidget t es a = forall e. Reflex.Reflex t => Reflex Dom t e -> Eff (e :& es) a

type BFWidgetIntern es a = forall e. Reflex Dom (SpiderTimeline Global) e -> Eff (e :& es) a

runReflexDom
  :: ei :> es
  => (JSM () -> IO ())
  -> BFWidget es ()
  -> BFWidget es ()
  -> IOE ei
  -> Eff es ()
runReflexDom = \runner html_head body io -> effIO io . runner $ mainWidgetWithHead (inWidget html_head) (inWidget body)

withDom
  :: e :> es
  => Reflex Dom t e
  -> (forall m r. (DomBuilder t m, MonadReflex t m) => m r -> m r)
  -> BFPartWidget t es a
  -> Eff es a
withDom = \r@ReflexHandle{payload = DomHandle{}} runner inner -> runWithDomData r (runner (inWidget inner))

elAttr :: e :> es => Reflex Dom t e -> Text -> Map Text Text -> BFPartWidget t es a -> Eff es a
elAttr r t attr = withDom r (Dom.elAttr t attr)

elClass :: e :> es => Reflex Dom t e -> Text -> Text -> BFPartWidget t es a -> Eff es a
elClass r t cls inner = elAttr r t ("class" =: cls) inner

elClss :: e :> es => Reflex Dom t e -> Text -> [Text] -> BFPartWidget t es a -> Eff es a
elClss r tg clss = elClass r tg (clss ^. re worded)

el :: e :> es => Reflex Dom t e -> Text -> BFWidget es a -> Eff es a
el r t inner = elAttr r t mempty inner

inWidget
  :: BFWidgetIntern es a
  -> (forall x. Widget x a)
inWidget = \act -> do
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
  ((a, jsmRequesterPost), requesterPost) <- liftIO $ unsafeUnEff do
    runState requesterPre \requesterStateHandle -> runState jsmRequesterPre \jsmRequesterState ->
      assoc1Eff $
        let
          r =
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
              , runWithReplaceImpl = \(ReflexAction initial) ev ->
                  runWithDomData r $
                    runWithReplace
                      (liftIO . unsafeUnEff $ initial r)
                      (ev <&> \(ReflexAction a) -> inWidget a)
              }
         in
          act r
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
  pure a

-- runReflexDomGUI
--   :: ei :> es
--   => BFWidget es ()
--   -> BFWidget es ()
--   -> IOE ei
--   -> Eff es ()
-- runReflexDomGUI = runReflexDom GTK.run

runReflexDomServer
  :: ei :> es
  => Int
  -> BFWidget es ()
  -> BFWidget es ()
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

runWithDomData
  :: forall t e es r. e :> es => Reflex Dom t e -> (forall x. Widget x r) -> Eff es r
runWithDomData ReflexHandle{spiderData, payload = d@(DomHandle env con _ _)} act = do
  modifyM @es d.jsmRequesterState \jsmPreState ->
    modifyM @es (spiderData.requesterStateHandle) \preState ->
      UnsafeMkEff $
        runWidget
          act
          spiderData.requesterSelector
          d.requesterSelector
          con
          spiderData.postBuild
          spiderData.triggerChan
          env
          jsmPreState
          preState

dom
  :: forall t e es r. e :> es => Reflex Dom t e -> (forall m. (DomBuilder t m, MonadReflex t m) => m r) -> Eff es r
dom rd@ReflexHandle{payload = DomHandle{}} act = runWithDomData rd act

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
