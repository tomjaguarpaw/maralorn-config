module Bluefin.Dialog.ReflexDom (runDomDialog) where

import Bluefin.Dialog
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Maralude
import Reflex
import Reflex.Dom.Core
import Reflex.Network (networkHold)

runDomDialog
  :: e :> es
  => IOE e
  -> ( forall ei
        . ei :> es
       => BFWidget es
       -> BFWidget es
       -> IOE ei
       -> Eff es ()
     )
  -> ( forall e1 e2 e3 ex t
        . (e1 :> ex, e2 :> ex, e3 :> ex, Reflex t)
       => IOE e1
       -> ReflexE t e2
       -> Dialog t e3
       -> Eff ex ()
     )
  -> Eff es ()
runDomDialog = \io engine app ->
  engine
    (\r d -> (runDomDialogHead r d))
    (\r d -> (runDomDialogBody r d (app io r)))
    io

runDomDialogHead
  :: forall er es t
   . (Reflex t, er :> es)
  => ReflexE t er
  -> Dom t er
  -> Eff es ()
runDomDialogHead = \r d -> dom r d $ do
  elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  el "style" $ text "html, body { background: black; height: 100%; }"

runDomDialogBody
  :: forall er es t
   . (Reflex t, er :> es)
  => ReflexE t er
  -> Dom t er
  -> (forall e. Dialog t e -> Eff (e :& es) ())
  -> Eff es ()
runDomDialogBody = \r d act ->
  inContext' . act $ MkDialog @t @er \ePage ->
    switchDyn <$> dom r d do networkHold (pure never) do ePage <&> renderPage

elClss :: DomBuilder t m => Text -> [Text] -> m a -> m a
elClss tg clss = elClass tg (clss ^. re worded)

renderPage :: (DomBuilder t m, MonadReflex t m) => Page a -> m (Event t a)
renderPage = \(MkPage rows) -> elClss
  "div"
  [ "h-full"
  , "p-2"
  , "text-4xl"
  , "lg:text-base"
  , "font-serif"
  , "text-white"
  , "lg:max-w-screen-sm"
  , "lg:mx-auto"
  , "bg-indigo-950"
  ]
  do
    evs <- forM rows \row ->
      el "div" $ forM row \case
        TextElement t -> do
          el "span" $ text t
          pure never
        ButtonElement label val -> do
          (e, _) <-
            elAttr'
              "button"
              ( "type"
                  =: "button"
                  <> "class"
                  =: ( [ "p-2"
                       , "m-2"
                       , "bg-indigo-800"
                       , "active:bg-indigo-200"
                       , "active:text-indigo-950"
                       ]
                        ^. re worded
                     )
              )
              $ text label
          pure $ domEvent Click e $> val
        _ -> error "not implemented"
    pure . leftmost . join $ evs
