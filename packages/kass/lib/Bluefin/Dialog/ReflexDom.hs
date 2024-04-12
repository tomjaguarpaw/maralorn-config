module Bluefin.Dialog.ReflexDom where

import Bluefin.Dialog
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Maralude
import Reflex
import Reflex.Dom.Core
import Reflex.Network (networkHold)

runDomDialog
  :: forall er es t
   . (Reflex t, er :> es)
  => ReflexE t er
  -> Dom t er
  -> (forall e. Dialog t e -> Eff (e :& es) ())
  -> Eff es ()
runDomDialog = \r d act ->
  inContext' . act $ MkDialog @t @er \ePage ->
    switchDyn <$> dom r d do networkHold (pure never) do ePage <&> renderPage

renderPage :: (DomBuilder t m, MonadReflex t m) => Page a -> m (Event t a)
renderPage = \(MkPage rows) -> do
  evs <- forM rows \row ->
    el "div" $ forM row \case
      TextElement t -> do
        el "span" $ text t
        pure never
      ButtonElement label val -> do
        (e, _) <- elAttr' "button" ("type" =: "button") $ text label
        pure $ domEvent Click e $> val
      _ -> error "not implemented"
  pure . leftmost . join $ evs
