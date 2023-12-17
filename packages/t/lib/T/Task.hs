module T.Task (module T.Task) where

import Control.Lens (folded, to, (^.), (^..))
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Time qualified as Time
import Relude
import Relude.Unsafe qualified as Unsafe
import Prelude ()

data TaskStatus = ToDo | Done | Deleted | Category | Maybe deriving stock (Show, Eq, Ord)

data Task = MkTask
  { status :: TaskStatus
  , description :: Text
  , tags :: Set Text
  , wait :: Maybe Time.Day
  , dep :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)

statusChars :: [(Char, TaskStatus)]
statusChars =
  [ ('o', ToDo)
  , ('x', Done)
  , ('-', Deleted)
  , ('*', Category)
  , ('?', Maybe)
  ]

printTask :: Task -> Text
printTask t =
  Text.unwords
    $ [ Text.singleton (Unsafe.fromJust (List.lookup (t ^. #status) (swap <$> statusChars)))
      , t ^. #description
      ]
    <> t
    ^.. #tags
      . folded
      . to ("+" <>)
      <> t
    ^.. #wait
      . folded
      . to (("wait:" <>) . show)
      <> t
    ^.. #dep
      . folded
      . to ("dep:" <>)
