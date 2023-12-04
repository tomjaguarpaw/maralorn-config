{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module T.Parser (module T.Parser) where

import Control.Lens (Prism', each, folded, itoList, makeFieldsNoPrefix, preview, to, view, (%=), (%~), (.~), (^.), (^..), (^?), _1, _Just, _Right)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time qualified as Time
import Relude
import Text.Megaparsec (MonadParsec (try), ParsecT, anySingle, choice, customFailure, manyTill)
import Text.Megaparsec.Char (char, hspace, hspace1, newline, string, tab)
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))
import Prelude ()

data TaskStatus = ToDo | Done | Deleted | Category | Maybe deriving stock (Show, Eq, Ord)

data Task = MkTask
  { _status :: TaskStatus
  , _description :: Text
  , _tags :: Set Text
  , _wait :: Maybe Time.Day
  }
  deriving stock (Show, Eq, Ord)

data FileElement = TaskEntry Task [FileElement] | Paragraph Text deriving stock (Show, Eq, Ord)

data SectionBody = MkSectionBody
  { _head :: [FileElement]
  , _sections :: [Section]
  }
  deriving stock (Show, Eq, Ord)

data Section = MkSection
  { _heading :: Text
  , _content :: SectionBody
  }
  deriving stock (Show, Eq, Ord)

makeFieldsNoPrefix ''Section
makeFieldsNoPrefix ''SectionBody
makeFieldsNoPrefix ''FileElement
makeFieldsNoPrefix ''Task

type LineParserT = ParsecT Void Text

type FileParserT = ParsecT Text [Line]

parseSectionBody :: Int -> FileParserT m SectionBody
parseSectionBody level = do
  head' <- many parseTopFileElement
  sections' <- many $ try do
    Heading lvl heading' <- anySingle
    when (lvl <= level) do customFailure "Heading has too low level"
    content' <- parseSectionBody (level + 1)
    pure do MkSection heading' content'
  pure do MkSectionBody head' sections'

parseTopFileElement :: FileParserT m FileElement
parseTopFileElement = do
  undefined

--    [do
--      _ <- string indent
--      (extraindent, task) <- parseTask
--      annotations <- many (parseFileElement (indent <> extraindent))
--    ,
--     do
--      manyTill (manyTill anySingle newline) blankline

data Whitespace = Space | Tab
  deriving stock (Show, Eq, Ord)

newtype Indent = MkIndent [Whitespace]
  deriving newtype (Show, Eq, Ord)

data Line = Blank | Heading Int Text | Task Indent Task | Other Indent Text
  deriving stock (Show, Eq, Ord)

blankLine :: FileParserT m ()
blankLine = do
  Blank <- anySingle
  pure ()

parseLine :: LineParserT m Line
parseLine =
  uncurry Heading <$> parseHeading <|> do
    indent <- parseIndent
    choice [Blank <$ newline, Task indent <$> parseTask, Other indent <$> restOfLine]

parseHeading :: LineParserT m (Int, Text)
parseHeading = do
  level <- length <$> some (char '#')
  _ <- parseIndent
  (level,) <$> restOfLine

parseWhitespace :: LineParserT m Whitespace
parseWhitespace = Space <$ char ' ' <|> Tab <$ tab

parseIndent :: LineParserT m Indent
parseIndent = MkIndent <$> many parseWhitespace

restOfLine :: LineParserT m Text
restOfLine = Text.pack <$> manyTill anySingle newline

parseTask :: LineParserT m Task
parseTask = do
  s <- parseStatus
  d <- restOfLine
  let d1, d2, tags', wait' :: [Text]
      (tags', d1) = List.partition (Text.isPrefixOf "+") $ Text.words d
      (wait', d2) = List.partition (Text.isPrefixOf "wait:") d1
  pure
    $ MkTask
      s
      (Text.unwords d2)
      (Set.fromList $ Text.drop 1 <$> tags')
      (wait' ^? folded . to (Text.drop 5) . to parseDate . _Just)

parseDate :: Text -> Maybe Time.Day
parseDate = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" . toString

parseStatus :: LineParserT m TaskStatus
parseStatus = choice statusParsers <* hspace1
 where
  statusParsers :: [LineParserT m TaskStatus]
  statusParsers =
    (\(c, s) -> s <$ char c)
      <$> [ ('o', ToDo)
          , ('x', Done)
          , ('-', Deleted)
          , ('*', Category)
          , ('?', Maybe)
          ]
