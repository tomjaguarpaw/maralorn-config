module T.File (module T.File) where

import Control.Lens (IndexedTraversal', Lens', Traversal', lens, prism', re, to, traversal, view, _1, _3)
import Control.Lens.Unsound (adjoin)
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.Text.Lens (IsText (packed))
import Maralude (Prism', (^.), (^..))
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

tasksInFile :: Text -> Traversal' SectionBody ([Text], Task)
tasksInFile label = tasksInSectionBody . addLabel (Text.splitOn "/" label)

tasksInSectionBody :: Traversal' SectionBody ([Text], Task)
tasksInSectionBody = adjoin (#head . traverse . tasksInFileElement) (#sections . traverse . tasksInSection)

tasksInSection :: Traversal' Section ([Text], Task)
tasksInSection f body = (#content . tasksInSectionBody . addLabel h) f body
 where
  h = [body ^. #heading]

tasksInFileElement :: Traversal' FileElement ([Text], Task)
tasksInFileElement = #_TaskEntry . ((_1 . withLabel) `adjoin` nestedTasksInFileElement)

nestedTasksInFileElement :: Traversal' (Task, a, [FileElement]) ([Text], Task)
nestedTasksInFileElement a b = (_3 . traverse . tasksInFileElement . addLabel lbl) a b
 where
  lbl :: [Text]
  lbl = [b ^. _1 . #description]

withLabel :: Lens' a ([b], a)
withLabel = lens ([],) (const snd)

addLabel :: [b] -> Lens' ([b], a) ([b], a)
addLabel label = lens (first (label <>)) (const (first (drop 1)))
