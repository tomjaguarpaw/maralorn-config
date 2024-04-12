module Bluefin.Dialog where

import Maralude
import Reflex

newtype Page a = MkPage (List (List (Element a)))
  deriving (Eq, Show)

data Element a where
  TextElement :: Text -> Element a
  ButtonElement :: Text -> a -> Element a
  FormElement :: (Text, a) -> Element (Text, a)

deriving instance Eq a => Eq (Element a)

deriving instance Show a => Show (Element a)

showPage :: e :> es => Dialog t e -> Event t (Page a) -> Eff es (Event t a)
showPage (MkDialog impl) page = useImpl do impl page

newtype Dialog t e = MkDialog (forall a. Event t (Page a) -> Eff e (Event t a))
