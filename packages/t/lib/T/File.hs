module T.File (module T.File) where

import Control.Lens (Traversal', to, view, _1)
import Control.Lens.Unsound (adjoin)
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.Text.Lens (IsText (packed))
import Relude
import T.Task (Task, printTask)
import Text.Megaparsec (TraversableStream (reachOffsetNoLine), VisualStream (showTokens))
import Prelude ()

type Indent = [Whitespace]

indent :: Traversal' Line [Whitespace]
indent = adjoin (#_Other . _1) (#_Task . _1)

data FileElement = TaskEntry Task Bool [FileElement] | Paragraph Text deriving stock (Show, Eq, Ord, Generic)

data SectionBody = MkSectionBody
  { head :: [FileElement]
  , sections :: [Section]
  }
  deriving stock (Show, Eq, Ord, Generic)

data Section = MkSection
  { heading :: Text
  , content :: SectionBody
  }
  deriving stock (Show, Eq, Ord, Generic)

data Whitespace = Space | Tab
  deriving stock (Show, Eq, Ord)

data Line = Blank | Heading Int Text | Task Indent Task | Other Indent Text
  deriving stock (Show, Eq, Ord, Generic)

instance VisualStream [Line] where
  showTokens _ = Text.unpack . printLines . toList

instance TraversableStream [Line] where
  reachOffsetNoLine _ = id

printLines :: [Line] -> Text
printLines = Text.unlines . fmap printLine

printLine :: Line -> Text
printLine = \case
  Blank -> ""
  Heading i t -> stimes i "#" <> " " <> t
  Task i t -> whitespace i <> printTask t
  Other i t -> whitespace i <> t

whitespace :: Indent -> Text
whitespace = view $ to (fmap \case Space -> ' '; Tab -> '\t') . packed
