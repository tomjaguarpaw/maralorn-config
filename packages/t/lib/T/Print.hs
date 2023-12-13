module T.Print (printFile) where

import Control.Lens (folded, over, to, (^.), (^..))
import Data.Generics.Labels ()
import Data.List qualified as List
import Maralude (lined)
import Relude
import T.File (FileElement (Paragraph, TaskEntry), Line (Blank, Heading, Other, Task), Section, SectionBody, Whitespace (Tab), indent, printLines)
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
