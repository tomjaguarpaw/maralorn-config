module Bluefin.Dialog.ReflexDom (runDomDialog) where

import Bluefin.Dialog hiding (text)
import Bluefin.Dialog.ReflexDom.TH (createCss)
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Language.Haskell.TH (bindCode)
import Language.Haskell.TH.Syntax (liftTyped)
import Maralude
import Reflex hiding (Reflex)
import Reflex qualified
import Reflex.Dom.Core hiding (Reflex, el, (.~))

runDomDialog
  :: e :> es
  => IOE e
  -> ( forall ei
        . ei :> es
       => BFWidget es ()
       -> BFWidget es ()
       -> IOE ei
       -> Eff es ()
     )
  -> ( forall e1 e2 ex t
        . (e1 :> ex, e2 :> ex, Reflex.Reflex t)
       => IOE e1
       -> Reflex Dialog t e2
       -> Eff ex ()
     )
  -> Eff es ()
runDomDialog = \io engine app ->
  engine
    (\r -> (runDomDialogHead r))
    (\r -> (runDomDialogBody r (app io)))
    io

runDomDialogHead
  :: forall er es t
   . (Reflex.Reflex t, er :> es)
  => Reflex Dom t er
  -> Eff es ()
runDomDialogHead = \r -> el r "style" $ \r' ->
  dom r'
    $ text [i|html, body { background: black; height: 100%; }\n#{css}|]

css :: Text
css = $$(bindCode createCss liftTyped)

runDomDialogBody
  :: forall er es t
   . (Reflex.Reflex t, er :> es)
  => Reflex Dom t er
  -> (forall e. Reflex Dialog t e -> Eff (e :& es) ())
  -> Eff es ()
runDomDialogBody = \r act ->
  elClss
    r
    "div"
    [ "p-2"
    , "absolute"
    , "inset-0"
    , "font-serif"
    , "text-white"
    , "lg:my-2"
    , "lg:rounded-lg"
    , "lg:max-w-screen-sm"
    , "lg:mx-auto"
    , "bg-blue-950"
    ]
    $ \r' -> act $ domToDialogHandle r'

domToDialogHandle :: Reflex.Reflex t => Reflex Dom t e -> Reflex Dialog t e
domToDialogHandle = go
 where
  go :: Reflex.Reflex t => Reflex Dom t e -> Reflex Dialog t e
  go = \r@ReflexHandle{runWithReplaceImpl} ->
    ReflexHandle
      { payload =
          DialogHandle \case
            TextElement txt -> dom r $ Reflex.Dom.Core.text txt
            ButtonElement label -> dom r $ domButton label
            PromptElement prompt default' -> dom r $ domInput prompt default'
            BreakElement -> el r "br" $ const blank
      , spiderData = r.spiderData
      , runWithReplaceImpl = \initial ev -> runWithReplaceImpl (mapAction initial) (mapAction <$> ev)
      }
  mapAction :: Reflex.Reflex t => ReflexAction Dialog t e b -> ReflexAction Dom t e b
  mapAction = \(ReflexAction act) -> (ReflexAction (act . go))

domInput :: (DomBuilder t m, MonadReflex t m) => Text -> Text -> m (Event t Text)
domInput = \prompt default' -> mdo
  active <- holdDyn False (True <$ eShow)
  ev' <-
    dyn
      $ active
      <&> \case
        False -> domButton prompt <&> ($> Left ())
        _ -> do
          input' <-
            _inputElement_value
              <$> inputElement
                ( def
                    & lensVL inputElementConfig_initialValue
                    .~ default'
                    & lensVL inputElementConfig_elementConfig
                    % lensVL elementConfig_initialAttributes
                    %~ ( <>
                          "class"
                            =: [ "bg-indigo-800"
                               , "p-1"
                               , "rounded-lg"
                               , "focus:bg-blue-900"
                               ]
                            ^. re worded
                       )
                )
          ev <- domButton "OK"
          pure $ Right <$> (current input' <@ ev)
  (eShow, eSend) <- fanEither <$> switchHold never ev'
  pure eSend

domButton :: DomBuilder t m => Text -> m (Event t ())
domButton = \label -> do
  (e, _) <-
    elAttr'
      "button"
      ( "type"
          =: "button"
          <> "class"
          =: ( [ "lg:p-0.5"
               , "p-2"
               , "m-1"
               , "rounded-lg"
               , "bg-indigo-800"
               , "active:bg-blue-900"
               ]
                ^. re worded
             )
      )
      $ text label
  pure $ domEvent Click e
