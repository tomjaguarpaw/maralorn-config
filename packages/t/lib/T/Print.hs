module T.Print (printFile, printTaskContext) where

import Control.Lens (folded, over, to, (^.), (^..))
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Text qualified as Text
import Maralude (lined)
import Relude
import T.File
  ( FileElement (Paragraph, TaskEntry)
  , Line (Blank, Heading, Other, Task)
  , Section
  , SectionBody
  , Whitespace (Tab)
  , indent
  , printLine
  , printLines
  )
import T.Query (TaskContext)
import T.Task (printTask)
import Prelude ()

printFile :: SectionBody -> Text
printFile =
  printLines
    . List.dropWhile (== Blank)
    . List.dropWhileEnd (== Blank)
    . printSectionBody 0

printSectionBody :: Int -> SectionBody -> [Line]
printSectionBody lvl sb =
  sb
    ^.. #head
      . folded
      . to printFileElement
      . folded
    <> sb
      ^.. #sections
        . folded
        . to (printSection (lvl + 1))
        . folded

printSection :: Int -> Section -> [Line]
printSection lvl s = Heading lvl (s ^. #heading) : Blank : printSectionBody lvl (s ^. #content)

printFileElement :: FileElement -> [Line]
printFileElement = \case
  TaskEntry t gap l -> Task [] t : ([Blank | gap]) <> fmap addIndent (printFileElement =<< l)
  Paragraph t -> t ^.. lined . folded . to (Other []) <> [Blank]

addIndent :: Line -> Line
addIndent = over indent (Tab :)

printTaskContext :: TaskContext -> Text
printTaskContext task_context =
  Text.unlines $
    printTask (task_context ^. #task)
      <-> Text.intercalate "." ((task_context ^. #file) : (task_context ^. #sections))
      : task_context ^.. #notes . folded . to printFileElement . folded . to addIndent . to printLine

(<->) :: Text -> Text -> Text
"" <-> b = b
a <-> "" = a
a <-> b | Text.isPrefixOf " " b || Text.isSuffixOf " " a = a <> b
a <-> b = a <> " " <> b
