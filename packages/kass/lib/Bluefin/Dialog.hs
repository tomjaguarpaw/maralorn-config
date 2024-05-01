module Bluefin.Dialog where

import Bluefin.Reflex
import Maralude
import Reflex hiding (Reflex)
import Reflex qualified

newtype Page a = MkPage {lines :: List (Line a)}
  deriving newtype (Semigroup, Monoid)
  deriving stock (Functor)

newtype Line a = MkLine {elems :: List (Element a)}
  deriving newtype (Semigroup, Monoid)
  deriving stock (Functor)

data Element a where
  TextElement :: Text -> Element a
  ButtonElement :: Text -> a -> Element a
  PromptElement :: Text -> Text -> (Text -> a) -> Element a
  deriving stock (Functor)

showPage :: (Reflex.Reflex t, e :> es) => Reflex Dialog t e -> Dynamic t (Page a) -> Eff es (Event t a)
showPage r@ReflexHandle{payload = DialogHandle{run}} page = do
  pb <- reflex r getPostBuild
  useImpl do run (leftmost [updated page, current page <@ pb])

newtype Dialog t e = DialogHandle
  { run :: forall a. Event t (Page a) -> Eff e (Event t a)
  }

line :: Line a -> Page a
line = MkPage . (: [])

txt :: Text -> Line a
txt = MkLine . (: []) . TextElement

button :: Text -> a -> Line a
button lbl val = MkLine [ButtonElement lbl val]

txtField :: Text -> Text -> (Text -> a) -> Line a
txtField lbl df f = MkLine [PromptElement lbl df f]
