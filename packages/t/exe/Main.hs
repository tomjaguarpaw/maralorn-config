{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens (each, folded, itoList, makeFieldsNoPrefix, to, view, (%=), (%~), (.~), (^.), (^..), (^?), _1, _Just, _Right)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time qualified as Time
import Relude
import System.Directory qualified as Dir
import System.FilePath ((</>))
import T.Parser qualified as Parser
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Prelude ()

data TaskStatus = ToDo | Done | Deleted | Category | Maybe deriving stock (Show, Eq)

data Task = MkTask
  { _status :: TaskStatus
  , _description :: Text
  , _tags :: [Text]
  , _wait :: Maybe Time.Day
  , _due :: Maybe Time.Day
  , -- modified :: UTCTime,
    _path :: [Text]
  }

makeFieldsNoPrefix ''Task

data ParseState = MkParseState
  { _file :: [Text]
  , _section :: [Text]
  , _parents :: [(Text, Text)]
  -- ^ Incremental Indentation and name of Task
  }

makeFieldsNoPrefix ''ParseState

main :: IO ()
main = do
  now <- Time.getCurrentTime
  getArgs >>= \case
    [] -> showTasks active
    ["unsorted"] -> showTasks (pall [active, pany [inbox, outdated now]])
    ["inbox"] -> showTasks (pall [null . (view tags), active])
    ["fmt"] -> putText . Parser.printFile =<< either fail pure . Parser.parseFile "stdin" =<< Text.IO.getContents
    x -> putStrLn $ "Unrecognized command: " <> show x

active :: Task -> Bool
active = (== ToDo) . view status

inbox :: Task -> Bool
inbox = elem "Inbox" . view path

outdated :: Time.UTCTime -> Task -> Bool
outdated now t = (t ^? (path . folded . to parseDate . _Just)) & maybe False (< now.utctDay)

pall :: [(a -> Bool)] -> a -> Bool
pall preds = \x -> all ($ x) preds

pany :: [(a -> Bool)] -> a -> Bool
pany preds = \x -> any ($ x) preds

showTasks :: (Task -> Bool) -> IO ()
showTasks pre = getTasks >>= printTree . filter pre

printList :: [Task] -> IO ()
printList = (mapM_ \t -> putTextLn $ printStatus (t ^. status) <> " " <> t ^. description <> " " <> Text.intercalate "." (t ^. path) <> " " <> Text.unwords (t ^. tags))

printTree :: [Task] -> IO ()
printTree = (mapM_ (uncurry printRow)) . (\t -> zip t ([] : ((^. path) <$> t))) . sortOn (^. path)
 where
  printRow = \cases
    task prepath -> putTextLn $ ((each .~ ' ') (Text.intercalate " " prefix) <> connection <> Text.intercalate "." own) <-> (" " <> printStatus (task ^. status) <-> (task ^. description) <-> Text.unwords (task ^. tags))
     where
      (prefix, own) = splitSharedPrefix prepath (task ^. path)
      connection
        | null prefix || null own = ""
        | otherwise = "."

"" <-> b = b
a <-> "" = a
a <-> b | Text.isPrefixOf " " b || Text.isSuffixOf " " a = a <> b
a <-> b = a <> " " <> b

-- >>> splitSharedPrefix "Foo" "Foobar"
splitSharedPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
splitSharedPrefix = \cases
  (a : as) (x : xs) | a == x -> splitSharedPrefix as xs & _1 %~ (x :)
  _ xs -> ([], xs)

parseDate :: Text -> Maybe Time.Day
parseDate = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" . toString

printStatus = \case
  ToDo -> "o"
  Done -> "x"
  Deleted -> "-"
  Category -> "*"
  Maybe -> "?"

getTasks :: IO [Task]
getTasks = do
  home <- Dir.getHomeDirectory
  let dir = (home <> "/git/notes")
  paths <- getFilePaths dir
  paths & fmap concat . mapM (\name -> parseFile (toText . drop (length dir + 1) $ name) . decodeUtf8 <$> readFileBS name)

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

type ParserT = P.ParsecT () Text

type Parser = ParserT Identity

parseLine :: ParserT (State ParseState) (Maybe Task)
parseLine = P.choice [heading, task]
 where
  indent = P.many P.spaceChar
  heading = do
    level <- length <$> P.some (P.char '#')
    P.space
    rest <- P.takeRest
    section %= (<> [rest]) . take (level - 1)
    pure Nothing
  status =
    P.choice
      ( (\(char, status) -> status <$ P.char char)
          <$> [ ('o', ToDo)
              , ('x', Done)
              , ('-', Deleted)
              , ('*', Category)
              , ('?', Maybe)
              ]
      )
      <* P.space1
  task = do
    i <- indent
    s <- status
    d <- P.takeRest
    state <- get
    let (tags, d1) = List.partition (Text.isPrefixOf "+") $ Text.words d
        (due, d2) = List.partition (Text.isPrefixOf "due:") d1
        (wait, d3) = List.partition (Text.isPrefixOf "wait:") d2
    pure
      $ Just
        ( MkTask
            s
            (Text.unwords d3)
            (Text.drop 1 <$> tags)
            (due ^? folded . to (Text.drop 4) . to parseDate . _Just)
            (wait ^? folded . to (Text.drop 5) . to parseDate . _Just)
            (state ^. file <> state ^. section)
        )

parseFile :: Text -> Text -> [Task]
parseFile name file =
  evalState
    ( forM
        (itoList $ lines file)
        (\(i, line) -> P.runParserT parseLine (toString name <> " line: " <> show i) line)
    )
    (MkParseState (Text.splitOn "/" name) [] [])
    ^.. folded . _Right . _Just
