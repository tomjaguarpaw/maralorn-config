{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Bluefin.Dialog.ReflexDom (runDomDialog) where

import Bluefin.Dialog
import Bluefin.Dialog.ReflexDom.TH (createCss)
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Language.Haskell.TH (bindCode)
import Language.Haskell.TH.Syntax (liftTyped)
import Maralude
import Reflex
import Reflex.Dom.Core hiding ((.~))
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
runDomDialogHead = \r d ->
  dom r d
    $ el "style"
    $ text [i|html, body { background: black; height: 100%; }\n#{css}|]

css :: Text
css = $$(bindCode createCss liftTyped)

runDomDialogBody
  :: forall er es t
   . (Reflex t, er :> es)
  => ReflexE t er
  -> Dom t er
  -> (forall e. Dialog t e -> Eff (e :& es) ())
  -> Eff es ()
runDomDialogBody = \r d act ->
  inContext'
    . act
    $ MkDialog
      { run = \ePage ->
          switchDyn <$> dom r d do networkHold (pure never) do ePage <&> renderPage
      , r
      }

elClss :: DomBuilder t m => Text -> [Text] -> m a -> m a
elClss tg clss = elClass tg (clss ^. re worded)

renderPage :: (DomBuilder t m, MonadReflex t m) => Page a -> m (Event t a)
renderPage = \page -> elClss
  "div"
  [ "p-2"
  , "absolute"
  , "inset-0"
  , "text-4xl"
  , "lg:text-base"
  , "font-serif"
  , "text-white"
  , "lg:my-2"
  , "lg:rounded-lg"
  , "lg:max-w-screen-sm"
  , "lg:mx-auto"
  , "bg-indigo-950"
  ]
  do
    evs <- forM page.lines \line' ->
      el "div" $ forM line'.elems \case
        TextElement t -> do
          el "span" $ text t
          pure never
        ButtonElement label val -> domButton label <&> ($> val)
        PromptElement prompt df process -> do
          text prompt
          input <-
            _inputElement_value
              <$> inputElement
                ( def
                    & lensVL inputElementConfig_initialValue
                    .~ df
                    & lensVL inputElementConfig_elementConfig
                    % lensVL elementConfig_initialAttributes
                    %~ ( <>
                          "class"
                            =: [ "bg-indigo-800"
                               , "p-1"
                               , "rounded-lg"
                               , "focus:bg-purple-900"
                               ]
                            ^. re worded
                       )
                )
          ev' <-
            dyn
              $ input
              <&> \case
                x | x == df -> pure never
                _ -> domButton "OK"
          ev <- switchHold never ev'
          pure $ process <$> current input <@ ev
    pure . leftmost . join $ evs

domButton :: DomBuilder t m => Text -> m (Event t ())
domButton = \label -> do
  (e, _) <-
    elAttr'
      "button"
      ( "type"
          =: "button"
          <> "class"
          =: ( [ "p-1"
               , "m-1"
               , "rounded-lg"
               , "bg-indigo-800"
               , "active:bg-purple-900"
               ]
                ^. re worded
             )
      )
      $ text label
  pure $ domEvent Click e
