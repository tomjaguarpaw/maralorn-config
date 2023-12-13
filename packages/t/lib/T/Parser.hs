{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module T.Parser (parseFile, printFile) where

import Control.Lens (Getting, Traversal', coerced, folded, foldl1Of, over, partsOf, preview, to, view, (^.), (^..), (^?), _1, _Just)
import Control.Lens.Unsound (adjoin)
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.List.NonEmpty (some1)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lens (IsText (packed))
import Data.Time qualified as Time
import Maralude (from, lined)
import Relude
import Relude.Unsafe (fromJust)
import Text.Megaparsec (MonadParsec (token, try), ParsecT, ShowErrorComponent (showErrorComponent), Stream (Token), TraversableStream (reachOffsetNoLine), VisualStream (showTokens), anySingle, choice, customFailure, manyTill, parse, skipMany)
import Text.Megaparsec.Char (char, hspace1, newline, tab)
import Prelude ()

parseFile :: String -> Text -> Either String SectionBody
parseFile name content =
  first displayException (parse (many parseLine) name content)
    >>= first displayException
    . parse (parseSectionBody 0) name

newtype MyParseFail = MkMyParseFail Text deriving (Eq, Ord, Show) via Text

instance ShowErrorComponent MyParseFail where
  showErrorComponent = toString @Text . coerce

printFile :: SectionBody -> Text
printFile = printLines . ensureTrailingNewline . printSectionBody 0

ensureTrailingNewline :: [Line] -> [Line]
ensureTrailingNewline = \case
  [] -> [Blank]
  [Blank] -> [Blank]
  (x : xs) -> x : ensureTrailingNewline xs

printSectionBody :: Int -> SectionBody -> [Line]
printSectionBody lvl sb =
  sb
    ^.. #head
      . folded
      . to printFileElement
      . folded
      <> sb
    ^.. #sections . folded . to (printSection (lvl + 1)) . folded

printFileElement :: FileElement -> [Line]
printFileElement = \case
  TaskEntry t gap l -> Task [] t : ([Blank | gap]) <> fmap addIndent (printFileElement =<< l)
  Paragraph t -> t ^.. lined . folded . to (Other []) <> [Blank]

addIndent :: Line -> Line
addIndent = over indent (Tab :)

indent :: Traversal' Line [Whitespace]
indent = adjoin (#_Other . _1) (#_Task . _1)

printSection :: Int -> Section -> [Line]
printSection lvl s = Heading lvl (s ^. #heading) : Blank : printSectionBody lvl (s ^. #content)

instance VisualStream [Line] where
  showTokens _ = Text.unpack . printLines . toList

instance TraversableStream [Line] where
  reachOffsetNoLine _ = id

data TaskStatus = ToDo | Done | Deleted | Category | Maybe deriving stock (Show, Eq, Ord)

data Task = MkTask
  { status :: TaskStatus
  , description :: Text
  , tags :: Set Text
  , wait :: Maybe Time.Day
  }
  deriving stock (Show, Eq, Ord, Generic)

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

type LineParserT = ParsecT Void Text

type FileParserT = ParsecT MyParseFail [Line]

blankLine :: FileParserT m ()
blankLine = parsePrism #_Blank

parseSectionBody :: Int -> FileParserT m SectionBody
parseSectionBody level = do
  skipMany blankLine
  head' <- many (parseFileElement Nothing)
  sections' <- many $ try do
    (lvl, heading') <- parsePrism #_Heading
    skipMany blankLine
    when (lvl <= level) do customFailure (MkMyParseFail "Heading is not indented enough")
    content' <- parseSectionBody (level + 1)
    pure do MkSection heading' content'
  pure do MkSectionBody head' sections'

parseFileElement :: Maybe Indent -> FileParserT m FileElement
parseFileElement in' =
  choice
    [ do
        (i, task) <- prismFunc #_Task
        gap <- not . List.null <$> many blankLine
        notes <- many (parseFileElement (Just i))
        pure $ TaskEntry task gap notes
    , do
        txt <- joinOtherLines <$> some1 (prismFunc #_Other)
        skipMany blankLine
        pure $ Paragraph txt
    ]
 where
  prismFunc :: Getting (First ([Whitespace], b)) Line ([Whitespace], b) -> FileParserT m ([Whitespace], b)
  prismFunc
    | Just i <- in' = parseIndentedPrism i
    | otherwise = parsePrism

type Indent = [Whitespace]

whitespace :: Indent -> Text
whitespace = view $ to (fmap \case Space -> ' '; Tab -> '\t') . packed

joinOtherLines :: NonEmpty (Indent, Text) -> Text
joinOtherLines l = l ^. partsOf (folded . to (uncurry line)) . from lined
 where
  prefix :: [Whitespace]
  prefix = foldl1Of (folded . _1 . coerced) commonPrefix l
  line i t = whitespace (unindent i) <> t
  unindent x = fromMaybe x $ List.stripPrefix prefix x

-- >>> commonPrefix "foo" "foot"
commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix = \cases
  (x : xs) (y : ys) | x == y -> x : commonPrefix xs ys
  _ _ -> []

parseIndentedPrism ::
  (MonadParsec e s m, Eq a) =>
  [a] ->
  Getting (First ([a], b)) (Token s) ([a], b) ->
  m ([a], b)
parseIndentedPrism i prism = token (guarded (biggerIndented i . fst) <=< preview prism) mempty

biggerIndented :: (Eq a) => [a] -> [a] -> Bool
biggerIndented = \cases
  pfx i | Just (_ : _) <- List.stripPrefix pfx i -> True
  _ _ -> False

data Whitespace = Space | Tab
  deriving stock (Show, Eq, Ord)

data Line = Blank | Heading Int Text | Task Indent Task | Other Indent Text
  deriving stock (Show, Eq, Ord, Generic)

printLines :: [Line] -> Text
printLines = Text.unlines . fmap printLine

printLine :: Line -> Text
printLine = \case
  Blank -> ""
  Heading i t -> stimes i "#" <> " " <> t
  Task i t -> whitespace i <> printTask t
  Other i t -> whitespace i <> t

printTask :: Task -> Text
printTask t =
  Text.unwords
    $ [ Text.singleton (fromJust (List.lookup (t ^. #status) (swap <$> statusChars)))
      , t ^. #description
      ]
    <> t
    ^.. #tags
      . folded
      . to ("+" <>)
      <> t
    ^.. #wait . folded . to (\day -> "wait:" <> show day)

-- MkTask
--   { _status :: TaskStatus,
--     _description :: Text,
--     _tags :: Set Text,
--     _wait :: Maybe Time.Day
--   }
--   deriving stock (Show, Eq, Ord)

parsePrism :: (Stream s, Ord e) => Getting (First a) (Token s) a -> ParsecT e s m a
parsePrism prism = token (preview prism) mempty

parseLine :: LineParserT m Line
parseLine =
  uncurry Heading <$> parseHeading <|> do
    i <- parseIndent
    choice [Blank <$ newline, Task i <$> try parseTask, Other i <$> restOfLine]

parseHeading :: LineParserT m (Int, Text)
parseHeading = do
  level <- length <$> some (char '#')
  _ <- parseIndent
  (level,) <$> restOfLine

parseWhitespace :: LineParserT m Whitespace
parseWhitespace = Space <$ char ' ' <|> Tab <$ tab

parseIndent :: LineParserT m Indent
parseIndent = many parseWhitespace

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
  statusParsers = statusChars <&> \(c, s) -> s <$ char c

statusChars :: [(Char, TaskStatus)]
statusChars =
  [ ('o', ToDo)
  , ('x', Done)
  , ('-', Deleted)
  , ('*', Category)
  , ('?', Maybe)
  ]
