module StatusScript.Modules.Vikunja (tasks, taskChar) where

import Data.Sequence qualified as Seq
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Network.Wreq (Options, param)
import Optics
import Reflex
import Reflex.Host.Headless qualified as R
import Relude
import StatusScript.Env
import StatusScript.Mode
import StatusScript.ReflexUtil (performEventThreaded, tickEvent)
import StatusScript.Warnings
import Vikunja (Task (..), defaultOptions, defaultProject, fetchAll, url)

todayBucket, weekBucket, doingBucket :: Int
todayBucket = 49
weekBucket = 48
doingBucket = 50

bucketOpts :: Int -> Options -> Options
bucketOpts bucket =
  (set (lensVL (param "filter_by")) ["bucket_id"])
    >>> set (lensVL (param "filter_value")) [show bucket]

getTasks :: Mode -> IO (Seq Warning)
getTasks mode = do
  opts <- defaultOptions
  fold
    [ on_mode (== Sort) $
        (taskWarnings "Inbox" inboxChar Count <$> fetchAll opts [i|#{url}/projects/-2/tasks|])
          <> (taskWarnings "Unsortiert" inboxChar Count <$> fetchAll opts [i|#{url}/projects/-3/tasks|])
    , on_mode (/= DND) $ taskWarnings "Checklisten" checklistChar Count <$> fetchAll opts [i|#{url}/projects/-4/tasks|]
    , taskWarnings "In Bearbeitung" taskChar Text
        <$> fetchAll (bucketOpts doingBucket opts) [i|#{url}/projects/#{defaultProject}/tasks|]
    , taskWarnings "Heute" taskChar None
        <$> fetchAll (bucketOpts todayBucket opts) [i|#{url}/projects/#{defaultProject}/tasks|]
    , on_mode (/= DND) $
        taskWarnings "Woche" taskChar None
          <$> fetchAll (bucketOpts weekBucket opts) [i|#{url}/projects/#{defaultProject}/tasks|]
    ]
 where
  on_mode f x = if f mode then x else pure mempty

checklistChar, taskChar, inboxChar :: Char
checklistChar = toEnum 0xf4a0 -- nf-oct-tasklist
taskChar = toEnum 0xe640 -- nf-seti-checkbox_unchecked
inboxChar = toEnum 987762

taskWarnings :: Text -> Char -> BarDisplay -> Seq (a, Task) -> Seq Warning
taskWarnings t c d = fmap (taskWarning t c d . snd) . Seq.sortOn (view (_2 % #kanban_position))

taskWarning :: Text -> Char -> BarDisplay -> Task -> Warning
taskWarning heading group' barDisplay t =
  MkWarning
    { description = t.title : ((toText [taskChar, ' ', ' '] <>) <$> parseChecklist t.description)
    , heading
    , barDisplay
    , group = group'
    , subgroup = Nothing
    }

parseChecklist :: Text -> [Text]
parseChecklist =
  fmap (fst . Text.breakOn "</p>" . Text.drop 3 . snd . Text.breakOn "<p>")
    . drop 1
    . Text.splitOn "<li data-checked=\"false\""

tasks :: R.MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
tasks env mode = do
  tick <- tag (current mode) <$> tickEvent 60
  ev <- performEventThreaded env (leftmost [updated mode, tick]) getTasks
  holdDyn mempty (toList <$> ev)
