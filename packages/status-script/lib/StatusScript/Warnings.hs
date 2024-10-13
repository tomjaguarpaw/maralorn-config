module StatusScript.Warnings (Warning (..), BarDisplay (..), warningSections, warningGroups) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NonEmpty
import Optics
import Relude

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

data WarningGroup = MkWarningGroup
  { name :: Char
  , count :: Int
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)

warningSections :: [Warning] -> [NonEmpty Warning]
warningSections =
  filter (has (#description % folded))
    >>> NonEmpty.groupBy (on (==) (.heading))

warningGroups :: [Warning] -> [WarningGroup]
warningGroups =
  filter (has (#barDisplay % #_Count))
    >>> fmap (.group)
    >>> sort
    >>> NonEmpty.group
    >>> fmap
      ( \group' ->
          MkWarningGroup
            { name = head group'
            , count = length group'
            }
      )
