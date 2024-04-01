module Bluefin.Reflex where

import Bluefin.IO (IOE)
import Maralude
import Reflex (Event)
import Reflex.Host.Headless (MonadHeadlessApp)
import Prelude ()

data Reflex t (ex :: Effects)

inReflex
  :: forall (e :: Effects) (es :: Effects) t r
   . e :> es
  => Reflex t e
  -> (forall (m :: Type -> Type). MonadHeadlessApp t m => m r)
  -> Eff es r
inReflex h act = undefined

withReflexHeadless
  :: forall a t (es :: Effects) (eio :: Effects)
   . eio :> es
  => IOE eio
  -> (forall (er :: Effects). Reflex t es -> Eff (er :& es) (Event t a))
  -> Eff es a
withReflexHeadless io act = undefined
