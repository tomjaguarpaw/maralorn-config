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
  :: (forall (er :: Effects) (eio :: Effects). IOE eio -> Reflex t es -> Eff (er :& eio) (Event t a))
  -> IO a
withReflexHeadless act = undefined
