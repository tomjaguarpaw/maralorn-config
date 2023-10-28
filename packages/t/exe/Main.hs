{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens (Prism', folded, itoList, preview, (^..), _Just, _Right)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Relude
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.Posix (getEnv)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Prelude ()

main :: IO ()
main = do
  getArgs >>= \case
    [] -> showTasks (== ToDo)
    ["inbox"] -> undefined
    ["all"] -> undefined
    ["list"] -> undefined

showTasks pre = getTasks >>= (mapM_ \t -> putTextLn $ printStatus t.status <> " " <> t.description <> " " <> Text.intercalate "." t.path) . filter (pre . (.status))

data TaskStatus = ToDo | Done | Deleted | Category | Maybe deriving (Show, Eq)

printStatus = \case
  ToDo -> "o"
  Done -> "x"
  Deleted -> "-"
  Category -> "*"
  Maybe -> "?"

data Task = MkTask
  { status :: TaskStatus
  , description :: Text
  , -- tags :: [Text],
    -- wait :: Maybe UTCTime,
    -- due :: Maybe UTCTime,
    -- modified :: UTCTime,
    path :: [Text]
  }

data ParseState = MkParseState
  { file :: [Text]
  , section :: [Text]
  , parents :: [(Text, Text)]
  -- ^ Incremental Indentation and name of Task
  }

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

parseLine :: P.ParsecT () Text (State ParseState) (Maybe Task)
parseLine = P.choice [heading, task]
 where
  indent = P.many P.spaceChar
  heading = do
    level <- length <$> P.some (P.char '#')
    rest <- P.takeRest
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
    pure $ Just (MkTask s d (state.file <> state.section))

parseFile :: Text -> Text -> [Task]
parseFile name file =
  evalState
    ( forM
        (itoList $ lines file)
        (\(i, line) -> P.runParserT parseLine (toString name <> " line: " <> show i) line)
    )
    (MkParseState (Text.splitOn "/" name) [] [])
    ^.. folded . _Right . _Just
