module StatusScript.Warnings (Warning (..), BarDisplay (..)) where

import Data.Aeson qualified as Aeson
import Maralorn.Prelude

data BarDisplay = None | Count | Text
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)

data Warning = MkWarning
  { description :: [Text]
  , heading :: Text
  , barDisplay :: BarDisplay
  , group :: Char
  , subgroup :: Maybe Char
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)
