module Main (main) where

import Control.Lens (over, toListOf, traversed)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time qualified as Time
import Maralude hiding (mapM, mapM_)
import Relude hiding (getArgs, putTextLn)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import T.File (FileElement (TaskEntry), SectionBody, tasksInFile)
import T.Parser (parseFile)
import T.Parser qualified as Parser
import T.Print (printFile)
import T.Task (Task, TaskStatus (Category, Maybe, ToDo), printTask)
import Prelude ()

main :: IO ()
main = do
  now <- Time.getCurrentTime
  getArgs >>= \case
    [] -> showTasks active (const True)
    ("tag" : tag) -> showTasks (pall [todo, Set.isSubsetOf (Set.fromList tag) . view (_2 . #tags)]) (const True)
    ["unsorted"] -> showTasks (pall [active, pany [inbox, outdated now]]) (const True)
    ["inbox"] -> showTasks todo (pall [has (#tags . only mempty), (`elem` [ToDo, Category]) . view #status])
    ["fmt"] -> putText . printFile =<< either fail pure . Parser.parseFile "stdin" =<< Text.IO.getContents
    x -> putStrLn $ "Unrecognized command: " <> show x

active :: ([Text], Task) -> Bool
active = (`elem` [ToDo, Category, Maybe]) . view (_2 . #status)

todo :: ([Text], Task) -> Bool
todo = has (_2 . #status . only ToDo)

inbox :: ([Text], Task) -> Bool
inbox = elem "Inbox" . view _1

outdated :: Time.UTCTime -> ([Text], Task) -> Bool
outdated now t = (t ^? (_1 . folded . to parseDate . _Just)) & maybe False (< now.utctDay)

pall :: [(a -> Bool)] -> a -> Bool
pall preds = \x -> all ($ x) preds

pany :: [(a -> Bool)] -> a -> Bool
pany preds = \x -> any ($ x) preds

showTasks :: (([Text], Task) -> Bool) -> (Task -> Bool) -> IO ()
showTasks pre scopePredicate = getTasks scopePredicate >>= printList . filter pre

printList :: [([Text], Task)] -> IO ()
printList = mapM_ \t -> putTextLn $ printTask (t ^. _2) <-> (t ^. _1 . to (Text.intercalate "."))

-- printTree :: [([Text], Task)] -> IO ()
-- printTree = (mapM_ (uncurry printRow)) . (\t -> zip t ([] : ((^. _1) <$> t)))
--  where
--   printRow :: ([Text], Task) -> [Text] -> IO ()
--   printRow = \cases
--     (path, task) prepath -> putTextLn $ ((each .~ ' ') (Text.intercalate " " prefix) <> connection <> Text.intercalate "." own) <-> (" " <> printTask task)
--      where
--       (prefix, own) = splitSharedPrefix prepath path
--       connection :: Text
--       connection
--         | null prefix || null own = ""
--         | otherwise = "."

(<->) :: Text -> Text -> Text
"" <-> b = b
a <-> "" = a
a <-> b | Text.isPrefixOf " " b || Text.isSuffixOf " " a = a <> b
a <-> b = a <> " " <> b

-- -- >>> splitSharedPrefix "Foo" "Foobar"
-- splitSharedPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
-- splitSharedPrefix = \cases
--   (a : as) (x : xs) | a == x -> splitSharedPrefix as xs & _1 %~ (x :)
--   _ xs -> ([], xs)

filterFile :: (Task -> Bool) -> SectionBody -> SectionBody
filterFile predicate = go
 where
  go :: SectionBody -> SectionBody
  go = over #head (mapMaybe goFileElement) . over (#sections . traversed . #content) go
  goFileElement :: FileElement -> Maybe FileElement
  goFileElement = \case
    TaskEntry t g n | predicate t -> Just $ TaskEntry t g (mapMaybe goFileElement n)
    _ -> Nothing

parseDate :: Text -> Maybe Time.Day
parseDate = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" . toString

getTasks :: (Task -> Bool) -> IO [([Text], Task)]
getTasks predicate = do
  home <- Dir.getHomeDirectory
  let dir = (home <> "/git/notes")
  paths <- getFilePaths dir
  paths & fmap concat . mapM \case
    n -> toListOf (folded . to (filterFile predicate) . tasksInFile (name ^. into)) . parseFile (name) <$> readFileUTF8 n
     where
      name = drop (length dir + 1) n

readFileUTF8 :: FilePath -> IO Text
readFileUTF8 = fmap decodeUtf8 . readFileBS

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths dir =
  Dir.listDirectory dir
    >>= fmap concat
    . mapM \name -> do
      isFile <- Dir.doesFileExist (dir </> name)
      isDir <- Dir.doesDirectoryExist (dir </> name)
      case () of
        _ | '.' `elem` name -> pure []
        _ | isFile -> pure [dir </> name]
        _ | isDir -> getFilePaths (dir </> name)
        _ -> pure []
