module T.Query (module T.Query) where

import Control.Lens (anyOf, filtered, toListOf)
import Data.Set qualified as Set
import Data.Time qualified as Time
import Maralude hiding (mapM, mapM_)
import Relude hiding (getArgs, putTextLn, take)
import T.File (FileElement, Section, SectionBody, tasksInSectionBody)
import T.Parser qualified as Parser
import T.Task (Task (status), TaskStatus (Category, Maybe, ToDo))

data TaskContext = MkTaskContext
  { file :: Text
  , sections :: [Text]
  , task :: Task
  , notes :: [FileElement]
  , allTasks :: [Task]
  , now :: Time.Day
  }
  deriving stock (Eq, Show, Generic)

data Query = MkQuery
  { take :: TaskContext -> Bool
  , descent :: TaskContext -> Bool
  }
  deriving stock (Generic)

simpleQuery :: (TaskContext -> Bool) -> Query
simpleQuery take = MkQuery{take, descent}
 where
  descent = not . take

hasTags :: Set Text -> Query
hasTags tags = simpleQuery $ pall [actionable, Set.isSubsetOf tags . view (#task . #tags)]

unsorted :: Query
unsorted = simpleQuery $ pall [active, pany [has (#file . only "Inbox"), outdated]]

active :: TaskContext -> Bool
active = anyOf (#task . #status) (`elem` [ToDo, Category, Maybe])

todo :: TaskContext -> Bool
todo = has (#task . #status . only ToDo)

inbox :: Query
inbox =
  MkQuery
    { take = pall [actionable, not . hasChildren, not . tagged, isNothing . hasDateFile]
    , descent = pall [relevantForInbox . view #task, not . inactive, not . tagged, isNothing . hasDateFile]
    }

inactive :: TaskContext -> Bool
inactive = pany [depToDo, waiting]

actionable :: TaskContext -> Bool
actionable = pall [todo, not . inactive]

waiting :: TaskContext -> Bool
waiting tc = has (#task . #wait . _Just . filtered (< tc ^. #now)) tc

depToDo :: TaskContext -> Bool
depToDo taskContext = case taskContext ^. #task . #dep of
  Nothing -> False
  Just dep ->
    has
      (#allTasks . folded . filtered (has (#status . only ToDo)) . #tags . folded . only dep)
      taskContext

tagged :: TaskContext -> Bool
tagged = anyOf (#task . #tags) (/= mempty)

relevantForInbox :: Task -> Bool
relevantForInbox = (`elem` [ToDo, Category]) . (.status)

hasChildren :: TaskContext -> Bool
hasChildren = anyOf (#notes . folded . #_TaskEntry . _1) relevantForInbox

outdated :: TaskContext -> Bool
outdated context = maybe False (< context.now) . hasDateFile $ context

hasDateFile :: TaskContext -> Maybe Time.Day
hasDateFile = Parser.parseDate . (.file)

pall :: [(a -> Bool)] -> a -> Bool
pall preds = \x -> all ($ x) preds

pany :: [(a -> Bool)] -> a -> Bool
pany preds = \x -> any ($ x) preds

query :: Query -> Time.Day -> [(Text, SectionBody)] -> [TaskContext]
query query' now files = toListOf (folded . to (uncurry queryFile) . folded) files
 where
  allTasks :: [Task]
  allTasks = files ^.. folded . _2 . tasksInSectionBody . _2
  queryFile :: Text -> SectionBody -> [TaskContext]
  queryFile file = goSectionBody []
   where
    goSectionBody :: [Text] -> SectionBody -> [TaskContext]
    goSectionBody section =
      toListOf $ #head . folded . to (goFileElement section) . folded <> #sections . folded . to (goSection section) . folded
    goSection :: [Text] -> Section -> [TaskContext]
    goSection section s = s ^. #content . to (goSectionBody (section <> [s ^. #heading]))
    goFileElement :: [Text] -> FileElement -> [TaskContext]
    goFileElement section = toListOf (#_TaskEntry . to (\(a, _, b) -> goTaskEntry section a b) . folded)
    goTaskEntry :: [Text] -> Task -> [FileElement] -> [TaskContext]
    goTaskEntry sections task notes =
      [context | (query' ^. #take) context]
        <> if (query' ^. #descent) context
          then goFileElement (sections <> [task ^. #description]) =<< notes
          else []
     where
      context = MkTaskContext{sections, file, task, notes, allTasks, now}
