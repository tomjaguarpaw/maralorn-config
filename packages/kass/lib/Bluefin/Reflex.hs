module Bluefin.Reflex where

import Bluefin.Internal (Eff (UnsafeMkEff), insertFirst, unsafeRemoveEff)
import Control.Monad.Fix (MonadFix)
import GHC.Records (HasField)
import Maralude
import Reflex
  ( MonadHold
  , MonadSample
  , SpiderTimeline
  , constDyn
  , current
  , runSpiderHostForTimeline
  , sample
  , withSpiderTimeline
  )
import Reflex qualified
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderTimelineEnv, spiderTimeline)

-- | Reflex Effect Handle
data Reflex t (es :: Effects) where
  MkReflex :: HasSpiderTimeline x => Reflex (SpiderTimeline x) es

type MonadReflex t m = (MonadHold t m, MonadSample t m, MonadFix m)

-- | Reflex Handler
runReflex
  :: eio :> es
  => IOE eio
  -> (forall t. Reflex.Reflex t => Reflex t er -> Eff (er :& es) a)
  -> Eff es a
runReflex io act =
  withEffInIO
    ( \runInIO -> withSpiderTimeline \(_ :: SpiderTimelineEnv x) ->
        runInIO $ unsafeRemoveEff . act $ MkReflex @x
    )
    io

{- | Reflex Actions
TODO: Add postBuild, triggerEvent and performEvent to the action (this will require extending the Handle type)
-}
inReflex
  :: e :> es
  => Reflex t e
  -> (forall (m :: Type -> Type). MonadReflex t m => m r)
  -> Eff es r
inReflex MkReflex act = UnsafeMkEff $ runSpiderHostForTimeline act spiderTimeline

-- Example App

main :: IO ()
main = runEff \io -> runReflex io (\reflex -> app $ MkEnv io reflex)

data Env t e1 e2 = MkEnv
  { io :: IOE e1
  , reflex :: Reflex t e2
  }

type WithIO env e es = (HasField "io" env (IOE e), e :> es)

type WithReflex t env e es = (Reflex.Reflex t, HasField "reflex" env (Reflex t e), e :> es)

app
  :: ( WithIO env e1 es
     , WithReflex t env e2 es
     )
  => env
  -> Eff es ()
app env = do
  let myDyn = constDyn 5
  five <- inReflex env.reflex $ sample (current myDyn)
  effIO env.io $ print five

-- Bluefin Helper

-- | Like withEffToIO but without running IO in the inner effect action.
withEffInIO :: e :> es => ((forall r. Eff es r -> IO r) -> IO a) -> IOE e -> Eff es a
withEffInIO f = withEffToIO (\runInIO -> f (\act -> runInIO $ const (insertFirst act)))
