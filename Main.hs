{-# LANGUAGE ViewPatterns, ScopedTypeVariables, NamedFieldPuns, OverloadedStrings, NoImplicitPrelude, ExtendedDefaultRules, QuasiQuotes, MultiWayIf #-}
module Main where

import           Data.String.Interpolate        ( i )
import           Relude                  hiding ( intercalate
                                                , zip
                                                )
import           Data.Text                      ( intercalate
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
import           System.Environment             ( getArgs )
-- TODO: use Text instead of linked lists of chars

type WeechatLog = [WeechatLine]
data WeechatLine = WeechatLine
  { wlDate :: Text
  , wlTime :: Text
  , wlNick :: Text
  , wlMsg  :: Text
  }
  deriving (Show, Eq, Ord)
-- TODO: specific handling of join/part/network messages

header :: Text
header = unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "  <head>"
  , "    <meta charset=\"UTF-8\" />"
  , "    <title>IRC log</title>"
  , "  </head>"
  ]

data LogFile = LogFile
  { path    :: Text
  , server  :: Text
  , channel :: Text
  }
  deriving (Show, Eq, Ord, Read)

type Parser = MP.Parsec Text Text

hyphen :: Parser Char
hyphen = MP.char '-'
parseDate :: Parser Text
parseDate = do
  year <- MP.count 4 MP.digitChar
  void hyphen
  month <- MP.count 2 MP.digitChar
  void hyphen
  day <- MP.count 2 MP.digitChar
  pure [i|#{year}-#{month}-#{day}|]
parseTime :: Parser Text
parseTime = do
  hour <- MP.count 2 MP.digitChar
  void $ MP.char ':'
  minute <- MP.count 2 MP.digitChar
  void $ MP.char ':'
  seconds <- MP.count 2 MP.digitChar
  pure [i|#{hour}:#{minute}:#{seconds}|]
dirSep :: Parser Char
dirSep = MP.char '/'
symbol :: Text -> Parser Text
symbol = MP.symbol MPC.space
folder :: Parser Text
folder = toText <$> MP.manyTill MP.asciiChar dirSep

matrixParser :: Text -> Parser LogFile
matrixParser p = do
  void $ MP.count 4 MP.digitChar -- year
  void dirSep
  prefix <- symbol "matrix:"
  server <- folder
  void folder -- room_id
  void parseDate
  void hyphen
  void $ symbol server
  void $ MP.char '.'
  channel <- toText <$> MP.manyTill MP.asciiChar (symbol ".weechatlog")
  pure $ LogFile p (prefix <> server) channel

ircParser :: Text -> Parser LogFile
ircParser p = do
  void $ MP.count 4 MP.digitChar
  void dirSep
  prefix  <- symbol "irc:" :: Parser Text
  server  <- folder
  channel <- folder
  void parseDate
  void $ symbol ".weechatlog"
  pure $ LogFile p (prefix <> server) channel

logFolder :: Text
logFolder = "/home/maralorn/logs/"

main :: IO ()
main = do
  now <- T.getCurrentTime
  let getFiles = \t p ->
        L.groupSortOn (\x -> (channel x, server x))
          .   mapMaybe ((\x -> MP.parseMaybe (p x) x) . toText)
          <$> getDirectoryFiles
                (toString logFolder)
                (   T.formatTime T.defaultTimeLocale t
                <$> [yesterday now, today now]
                )
  matrixFiles <- getFiles "%Y/matrix:*/*.!*/%Y-%m-%d-*.weechatlog" matrixParser
  ircFiles    <- getFiles "%Y/irc:*/#*/%Y-%m-%d.weechatlog" ircParser
  logs        <- mapM readLogFiles $ mapMaybe nonEmpty $ matrixFiles <> ircFiles
  let entries = logs & mapMaybe (logToFeedEntry now)
      feed    = nullFeed [i|weechat-logs-#{timestamp now}|]
                         (TextString "Weechat Logs")
                         (timestamp now)
  [pathToWrite] <- getArgs
  whenJust (textFeed feed { feedEntries = entries })
    $ \file -> writeFileLText pathToWrite file

today :: T.UTCTime -> T.Day
today = T.utctDay
yesterday :: T.UTCTime -> T.Day
yesterday = T.addDays (negate 1) . today

timestamp :: T.UTCTime -> Text
timestamp = toText . T.formatTime T.defaultTimeLocale "%Y-%m-%d %H:%M"

logToFeedEntry :: T.UTCTime -> Log -> Maybe Entry
logToFeedEntry now =
  \Log { logchannel, logserver, messages = filter msgFilter -> messages } ->
    if not (null messages)
      then Just (nullEntry [i|#{logserver}-#{logchannel}-#{timestamp now}|]
                           (TextString [i|#{logchannel} - (#{logserver})|])
                           (timestamp now)
                )
        { entryContent = Just $ HTMLContent $ printHTML messages
        }
      else Nothing
 where
  cutoff =
    toText $ T.formatTime T.defaultTimeLocale "%Y-%m-%d 19:50" $ yesterday now
  msgFilter msg = [i|#{wlDate msg} #{wlTime msg}|] >= cutoff

data Log = Log
  { logchannel :: Text
  , logserver  :: Text
  , messages   :: [WeechatLine]
  }
  deriving (Show, Eq, Ord)

readLogFiles :: NonEmpty LogFile -> IO Log
readLogFiles files =
  readLogFile (head files)
    <$> mapM (readFileText . toString . (logFolder <>) . path) files


readLogFile :: LogFile -> NonEmpty Text -> Log
readLogFile LogFile { channel, server } contents = Log
  { logchannel = channel
  , logserver  = server
  , messages   = L.sortOn (\x -> (wlDate x, wlTime x))
                 .   concat
                 $   parseWeechatLog
                 <$> contents
  }

parseWeechatLine :: Parser WeechatLine
parseWeechatLine = do
  date <- parseDate
  void $ MP.char ' '
  time <- parseTime
  void MP.tab
  nick <- toText <$> MP.manyTill MP.printChar MP.tab
  WeechatLine date time nick <$> MP.takeRest

parseWeechatLog :: Text -> [WeechatLine]
parseWeechatLog = filter actualMessage . mapMaybe parseLine . lines
 where
  actualMessage = not . (`elem` ["-->", "<--", "--"]) . wlNick
  parseLine     = MP.parseMaybe parseWeechatLine

printHTML :: [WeechatLine] -> Text
printHTML log =
  intercalate "\n"
    $  [header, "<body>"]
    ++ map printDay days
    ++ ["</body>", "</html>"]
 where
  days = groupBy ((==) `on` wlDate) log
  printDay ls =
    intercalate "\n" $ ["<h3>" <> wlDate (head ls) <> "</h3>"] <> toList
      (printRow <$> zip (WeechatLine "" "" "" "" :| toList ls) ls)
  printRow :: (WeechatLine, WeechatLine) -> Text
  printRow (prevRow, curRow) =
    "<i>"
      <> time
      <> "</i> <b>"
      <> printNick
      <> "</b> "
      <> message
      <> "<br>"
   where
    prevTime = Text.take 5 $ wlTime prevRow
    curTime  = Text.take 5 $ wlTime curRow
    prevNick = wlNick prevRow
    curNick  = wlNick curRow
    time | prevTime == curTime = ""
         | otherwise           = curTime
    nick | specialNick curNick = curNick
         | prevNick == curNick = ""
         | otherwise           = curNick
    printNick = Text.dropWhile (`elem` ['&', '@']) nick
    msg       = wlMsg curRow
    message
      | not (Text.null msg) && Text.head msg == '>'
      = "|<i style='color: grey'>" <> escape (Text.tail msg) <> "</i>"
      | otherwise
      = escape msg
  specialNick = (`elem` ["-->", "<--", "--", "*"])
  escape      = replace "<" "&lt;" . replace ">" "&gt;"
