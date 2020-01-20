{-# LANGUAGE ViewPatterns, ScopedTypeVariables, NamedFieldPuns, OverloadedStrings, NoImplicitPrelude, ExtendedDefaultRules, QuasiQuotes #-}
module Main where

import           Data.Char
import           Data.Function
import           Data.String.Interpolate        ( i )
import           Relude                  hiding ( intercalate
                                                , dropWhile
                                                , zip
                                                )
import qualified Relude.Unsafe                 as Unsafe
import           Data.Text                      ( intercalate
                                                , dropWhile
                                                , replace
                                                )
import           Text.Atom.Feed.Export          ( textFeed )
import qualified Data.Text                     as Text
import           Data.List.NonEmpty             ( groupBy
                                                , zip
                                                )
import           Text.Atom.Feed
import qualified Data.Time.Calendar            as T
import qualified Data.Time.Clock               as T
import qualified Data.Time.Format              as T
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char          as MPC
import qualified Text.Megaparsec.Char.Lexer    as MP
import           System.FilePattern.Directory   ( getDirectoryFiles )
import qualified Data.List.Extra               as L
import qualified Data.Set                      as Set
import           System.Environment             ( getArgs )
-- TODO: use Text instead of linked lists of chars

type WeechatLog = [WeechatLine]
data WeechatLine = WeechatLine { wlDate :: Text
                               , wlTime :: Text
                               , wlNick :: Text
                               , wlMsg :: Text } deriving (Show, Eq, Ord)
-- TODO: specific handling of join/part/network messages

header = unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "  <head>"
  , "    <meta charset=\"UTF-8\" />"
  , "    <title>IRC log</title>"
  , "  </head>"
  ]

data LogFile = LogFile { path :: Text, server :: Text, channel :: Text } deriving (Show, Eq, Ord, Read)

type Parser = MP.Parsec Text Text

hyphen = MP.char '-'
date = do
  MP.decimal
  hyphen
  MP.decimal
  hyphen
  MP.decimal
dirSep = MP.char '/'
symbol = MP.symbol MPC.space
folder = toText <$> MP.manyTill MP.asciiChar dirSep

matrixParser :: Text -> Parser LogFile
matrixParser = \p -> do
  MP.decimal -- year
  dirSep
  prefix <- symbol "matrix:"
  server <- folder
  folder -- room_id
  date
  hyphen
  symbol server
  MP.char '.'
  channel <- toText <$> MP.manyTill MP.asciiChar (symbol ".weechatlog")
  pure $ LogFile p (prefix <> server) channel

ircParser :: Text -> Parser LogFile
ircParser = \p -> do
  MP.decimal -- year
  dirSep
  prefix  <- symbol "irc:" :: Parser Text
  server  <- folder
  channel <- folder
  date
  symbol ".weechatlog"
  pure $ LogFile p (prefix <> server) channel

logFolder = "/home/maralorn/logs/"

main = do
  now <- T.getCurrentTime
  let today     = T.utctDay now
      yesterday = T.addDays (0 - 1) today
      getFiles  = \t p ->
        L.groupSortOn (\x -> (channel x, server x))
          .   mapMaybe (\x -> (MP.parseMaybe (p x) x))
          .   fmap toText
          <$> getDirectoryFiles
                (toString logFolder)
                (T.formatTime T.defaultTimeLocale t <$> [yesterday, today])
  matrixFiles <- getFiles "%Y/matrix:*/*.!*/%Y-%m-%d-*.weechatlog" matrixParser
  ircFiles    <- getFiles "%Y/irc:*/#*/%Y-%m-%d.weechatlog" ircParser
  logs        <- mapM readLogFiles $ mapMaybe nonEmpty $ matrixFiles <> ircFiles
  --(flip mapM_) logs $ \(Log { logchannel, logserver, messages }) -> do
  let
    timestamp = toText $ T.formatTime T.defaultTimeLocale "%Y-%m-%d %H:%M" now
    cutoff =
      toText $ T.formatTime T.defaultTimeLocale "%Y-%m-%d 19:55" yesterday
    msgFilter msg = [i|#{wlDate msg} #{wlTime msg}|] >= cutoff
    entries = logs & mapMaybe
      (\(Log { logchannel, logserver, messages = filter msgFilter -> messages }) ->
        if length messages > 0
          then Just (nullEntry
                      [i|#{logserver}-#{logchannel}-#{timestamp}|]
                      (TextString [i|#{logchannel} - (#{logserver})|])
                      timestamp
                    )
            { entryContent = Just $ HTMLContent $ printHTML messages
            }
          else Nothing
      )
    feed = nullFeed [i|weechat-logs-#{timestamp}|]
                    (TextString "Weechat Logs")
                    timestamp
  [pathToWrite] <- getArgs
  whenJust (textFeed feed { feedEntries = entries })
    $ \file -> writeFileLText pathToWrite file


data Log = Log { logchannel :: Text, logserver :: Text, messages :: [WeechatLine] } deriving (Show, Eq, Ord)

readLogFiles :: NonEmpty LogFile -> IO Log
readLogFiles files =
  readLogFile (head files)
    <$> mapM (readFileText . toString . (logFolder <>) . path) files


readLogFile :: LogFile -> NonEmpty Text -> Log
readLogFile = \LogFile { channel, server } contents -> Log
  { logchannel = channel
  , logserver  = server
  , messages   = L.sortOn (\x -> (wlDate x, wlTime x))
                 .   concat
                 $   parseWeechatLog
                 <$> contents
  }

parseWeechatLog :: Text -> [WeechatLine]
parseWeechatLog = mapMaybe parseWeechatLine . lines
 where
  parseWeechatLine l
    | [date, time, nick] <- take 3 . words $ l
    = let
        msg =
          drop (length (toString $ unwords [date, time, nick]) + 1) (toString l)
      in  if (nick `elem` ["-->", "<--", "--"])
            then Nothing
            else Just (WeechatLine date time nick (toText msg))
    | otherwise
    = trace ([i|Couldn‘t parse line #{show l}|]) Nothing

printHTML :: [WeechatLine] -> Text
printHTML log =
  intercalate "\n"
    $  [header, "<body>"]
    ++ map printDay days
    ++ ["</body>", "</html>"]
 where
  allNicks = Set.fromList . map (dropWhile sigil . wlNick) $ log
  days     = groupBy ((==) `on` wlDate) log
  printDay ls =
    intercalate ""
      $  ["<h3>" <> wlDate (head ls) <> "</h3>"]
      <> (toList $ printRow <$> zip (WeechatLine "" "" "" "" :| toList ls) ls)
  printRow :: (WeechatLine, WeechatLine) -> Text
  printRow (prevRow, curRow) =
    "<i style='color: grey'>"
      <> wlTime curRow
      <> " "
      <> nick
      <> "</i> "
      <> (escape $ wlMsg curRow)
      <> "<br>"
   where
    prevNick = wlNick prevRow
    curNick  = wlNick curRow
    nick | specialNick curNick = curNick
         | prevNick == curNick = " "
         | otherwise           = curNick

specialNick = (`elem` ["-->", "<--", "--", "*"])

sigil :: Char -> Bool
sigil = (`elem` ("@%+" :: String))
-- Weechat default nick hash function = sum of unicode values
hash :: Text -> Text
hash = show . (`mod` (length colors)) . sum . map ord . toString

colors =
  [ "cyan"
  , "magenta"
  , "green"
  , "brown"
  , "lightblue"
  , "default"
  , "lightcyan"
  , "lightmagenta"
  , "lightgreen"
  , "blue"
  ]

colorhl allNicks msg
  | firstWord == ""
  = msg
  | Text.last firstWord == ':' && nick `Set.member` allNicks
  = sigils
    <> "<span class=\"nc-color-"
    <> hash nick
    <> "\">"
    <> nick
    <> "</span>:"
    <> rest
  | otherwise
  = msg
 where
  (firstWord, rest ) = Text.span (not . isSpace) msg
  (sigils   , nick') = Text.span sigil firstWord
  nick               = Text.init nick'

escape = replace "<" "&lt;" . replace ">" "&gt;"
