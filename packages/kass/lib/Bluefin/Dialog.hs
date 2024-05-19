module Bluefin.Dialog where

import Bluefin.Reflex
import Maralude
import Reflex hiding (Reflex)

data Element t a where
  TextElement :: Text -> Element t ()
  ButtonElement :: Text -> Element t (Event t ())
  PromptElement :: Text -> Text -> Element t (Event t Text)
  BreakElement :: Element t ()

render :: forall t e es a. e :> es => Reflex Dialog t e -> Element t a -> Eff es a
render d e = case d.payload of DialogHandle{render = r} -> useImpl $ r e

newtype Dialog t e = DialogHandle
  { render :: forall a. Element t a -> Eff e a
  }
