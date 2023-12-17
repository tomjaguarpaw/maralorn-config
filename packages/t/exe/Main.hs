module Main (main) where

import Control.Lens (foldrOf, toListOf)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time qualified as Time
import Maralude hiding (mapM, mapM_)
import Relude hiding (getArgs, putTextLn, take)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import T.File (SectionBody)
import T.Parser (parseFile)
import T.Parser qualified as Parser
import T.Print (printFile, printTaskContext)
import T.Query (Query, TaskContext, hasTags, inbox, query, simpleQuery, todo, unsorted)
import Prelude ()

main :: IO ()
main = do
  getArgs >>= \case
    [] -> showTasks $ simpleQuery (const True)
    ["tags"] ->
      mapM_ (\(tag, count :: Natural) -> putTextLn $ tag <> " " <> show count)
        . sortOn (Down . snd)
        . Map.toList
        . foldrOf (folded . #task . #tags . folded . to (`Map.singleton` 1)) (Map.unionWith (+)) mempty
        =<< runQuery (simpleQuery todo)
    ("tag" : tags) -> showTasks $ hasTags (Set.fromList tags)
    ["unsorted"] -> showTasks unsorted
    ["inbox"] -> showTasks inbox
    ["fmt"] -> putText . printFile =<< either fail pure . Parser.parseFile "stdin" =<< Text.IO.getContents
    x -> putStrLn $ "Unrecognized command: " <> show x

runQuery :: Query -> IO [TaskContext]
runQuery q = do
  now <- Time.getZonedTime
  getTaskFiles <&> query q (now.zonedTimeToLocalTime.localDay)

showTasks :: Query -> IO ()
showTasks q = do
  runQuery q >>= putText . Text.concat . fmap printTaskContext

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

-- -- >>> splitSharedPrefix "Foo" "Foobar"
-- splitSharedPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
-- splitSharedPrefix = \cases
--   (a : as) (x : xs) | a == x -> splitSharedPrefix as xs & _1 %~ (x :)
--   _ xs -> ([], xs)

getTaskFiles :: IO [(Text, SectionBody)]
getTaskFiles = do
  home <- Dir.getHomeDirectory
  let dir = (home <> "/git/notes")
  paths <- getFilePaths dir
  paths & fmap concat . mapM \case
    n -> toListOf (to (parseFile name) . _Right . to (Text.pack name,)) <$> readFileUTF8 n
     where
      name :: [Char]
      name = (\x -> List.take (length x - 2) x) $ drop (length dir + 1) n

readFileUTF8 :: FilePath -> IO Text
readFileUTF8 = fmap decodeUtf8 . readFileBS

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths dir =
  Dir.listDirectory dir
    >>= fmap concat
    . mapM \name -> do
      isFile <- Dir.doesFileExist (dir </> name)
      isDir <- Dir.doesDirectoryExist (dir </> name)
      if
        | isFile, ".t" `List.isSuffixOf` name -> pure [dir </> name]
        | isDir -> getFilePaths (dir </> name)
        | otherwise -> pure []
