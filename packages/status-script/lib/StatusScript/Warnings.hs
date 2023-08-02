module StatusScript.Warnings (Warning (..)) where

import Data.Aeson qualified as Aeson
import Maralorn.Prelude

data Warning = MkWarning
  { description :: Text
  , group :: Text
  , subgroup :: Maybe Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)
