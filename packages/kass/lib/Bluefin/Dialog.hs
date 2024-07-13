module Bluefin.Dialog where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Reflex
import Reflex hiding (Reflex)
import Relude

data Element t a where
  TextElement :: Text -> Element t ()
  ButtonElement :: Text -> Element t (Event t ())
  PromptElement :: Text -> Text -> Element t (Event t Text)
  BreakElement :: Element t ()

render :: e :> es => Reflex Dialog t e -> Element t a -> Eff es a
render = \d e -> case d.payload of DialogHandle run -> useImpl $ run e

newtype Dialog t e = DialogHandle (forall a. Element t a -> Eff e a)

text :: e :> es => Reflex Dialog t e -> Text -> Eff es ()
text = \r lbl -> render r (TextElement lbl)

newline :: e :> es => Reflex Dialog t e -> Eff es ()
newline = \r -> render r (BreakElement)

input :: e :> es => Reflex Dialog t e -> Text -> Text -> Eff es (Event t Text)
input = \r lbl default' -> render r (PromptElement lbl default')

button :: e :> es => Reflex Dialog t e -> Text -> Eff es (Event t ())
button = \r lbl -> render r (ButtonElement lbl)
