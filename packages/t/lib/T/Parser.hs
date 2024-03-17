module T.Parser (parseFile, parseDate) where

import Control.Lens (Getting, coerced, folded, foldl1Of, partsOf, preview, to, (^.), (^?), _1, _Just)
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.List.NonEmpty (some1)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time qualified as Time
import Maralude (from, lined)
import Relude
import T.File
  ( FileElement (Paragraph, TaskEntry)
  , Indent
  , Line (..)
  , Section (MkSection)
  , SectionBody (MkSectionBody)
  , Whitespace (..)
  , whitespace
  )
import T.Task (Task (MkTask), TaskStatus, statusChars)
import Text.Megaparsec
  ( MonadParsec (eof, token, try)
  , ParsecT
  , ShowErrorComponent (showErrorComponent)
  , Stream (Token)
  , anySingle
  , choice
  , customFailure
  , manyTill
  , parse
  , skipMany
  )
import Text.Megaparsec.Char (char, hspace1, tab)
import Prelude ()

parseFile :: String -> Text -> Either String SectionBody
parseFile name content =
  mapM (first displayException . parse parseLine name) (Text.lines content)
    >>= first displayException
    . parse (parseSectionBody 0) name

newtype MyParseFail = MkMyParseFail Text deriving (Eq, Ord, Show) via Text

instance ShowErrorComponent MyParseFail where
  showErrorComponent = toString @Text . coerce

type LineParserT = ParsecT MyParseFail Text

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

joinOtherLines :: NonEmpty (Indent, Text) -> Text
joinOtherLines l = l ^. partsOf (folded . to (uncurry line)) . from lined
 where
  prefix :: [Whitespace]
  prefix = foldl1Of (folded . _1 . coerced) commonPrefix l
  line i t = whitespace (unindent i) <> t
  unindent x = fromMaybe x $ List.stripPrefix prefix x

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix = \cases
  (x : xs) (y : ys) | x == y -> x : commonPrefix xs ys
  _ _ -> []

parseIndentedPrism
  :: (MonadParsec e s m, Eq a)
  => [a]
  -> Getting (First ([a], b)) (Token s) ([a], b)
  -> m ([a], b)
parseIndentedPrism i prism = token (guarded (biggerIndented i . fst) <=< preview prism) mempty

biggerIndented :: Eq a => [a] -> [a] -> Bool
biggerIndented = \cases
  pfx i | Just (_ : _) <- List.stripPrefix pfx i -> True
  _ _ -> False

parsePrism :: MonadParsec e s m => Getting (First a) (Token s) a -> m a
parsePrism prism = token (preview prism) mempty

------------------------------------------------------------------------------
--- Line Parsing
------------------------------------------------------------------------------

parseLine :: LineParserT m Line
parseLine =
  uncurry Heading <$> parseHeading <|> do
    i <- parseIndent
    choice [Blank <$ eof, Task i <$> try parseTask, Other i <$> restOfLine]

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
restOfLine = Text.pack <$> manyTill anySingle eof

parseTask :: LineParserT m Task
parseTask = do
  s <- parseStatus
  d <- restOfLine
  let d1, d2, tags', wait' :: [Text]
      (tags', d1) = List.partition (Text.isPrefixOf "+") $ Text.words d
      (wait', d2) = List.partition (Text.isPrefixOf "wait:") d1
      (dep', d3) = List.partition (Text.isPrefixOf "dep:") d2
  pure
    $ MkTask
      s
      (Text.unwords d3)
      (Set.fromList $ Text.toLower . Text.drop 1 <$> tags')
      (wait' ^? folded . to (Text.drop 5) . to parseDate . _Just)
      (dep' ^? folded . to (Text.toLower . Text.drop 4))

parseDate :: Text -> Maybe Time.Day
parseDate = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" . toString

parseStatus :: LineParserT m TaskStatus
parseStatus = choice statusParsers <* hspace1
 where
  statusParsers :: [LineParserT m TaskStatus]
  statusParsers = statusChars <&> \(c, s) -> s <$ char c
