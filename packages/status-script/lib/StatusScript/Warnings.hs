module StatusScript.Warnings (Warning (..)) where

import Data.Aeson qualified as Aeson
import Maralorn.Prelude

data Warning = MkWarning
  { description :: Maybe Text
  , group :: Char
  , subgroup :: Maybe Char
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)
