module Main (main) where

import Data.List.Extra qualified as List
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Network.Wreq qualified as Wreq
import Relude
import Relude.Extra ((^.))
import System.FilePath qualified as FilePath
import System.FilePath.Posix ((<.>))
import Text.Atom.Feed qualified as Feed
import Text.Atom.Feed.Export qualified as Feed
import Text.HTML.TagSoup qualified as TagSoup
import Witch (into)

data Entry = MkEntry
  { link :: Text
  , title :: Text
  , time :: UTCTime
  }

type Tag = TagSoup.Tag Text

extractTimeFromTableRow :: [Tag] -> Maybe UTCTime
extractTimeFromTableRow =
  ( dropWhile (not . isDateTag)
      <&> find TagSoup.isTagText
  )
    >=> TagSoup.maybeTagText
    >=> parseTime "%Y-%b-%d %H:%M"

isDateTag :: Tag -> Bool
isDateTag = \tag ->
  TagSoup.isTagOpenName "td" tag
    && "date" == TagSoup.fromAttrib "class" tag

extractFolderFromTableRow :: [Tag] -> Maybe (Text, Text)
extractFolderFromTableRow =
  filter (TagSoup.isTagOpenName "a")
    <&> List.firstJust extractFolderFromATag

extractFolderFromATag :: Tag -> Maybe (Text, Text)
extractFolderFromATag = \tag ->
  let
    title = TagSoup.fromAttrib "title" tag
    href = TagSoup.fromAttrib "href" tag
   in
    if Text.null title || Text.null href
      then Nothing
      else Just (href, title)

parseTime :: Text -> Text -> Maybe UTCTime
parseTime = \format time_text ->
  Time.parseTimeM
    True
    Time.defaultTimeLocale
    (toString format)
    (toString time_text)

extractEntryFromTableRow :: [Tag] -> Maybe Entry
extractEntryFromTableRow = \row ->
  (\(link, title) time -> MkEntry{..})
    <$> (extractFolderFromTableRow row)
    <*> (extractTimeFromTableRow row)

fetchIndex :: Text -> IO [Entry]
fetchIndex = \url ->
  mapMaybe extractEntryFromTableRow
    . List.split (TagSoup.isTagOpenName "tr")
    . dropWhile (not . TagSoup.isTagOpenName "tbody")
    . TagSoup.parseTags
    . decodeUtf8
    . (^. Wreq.responseBody)
    <$> Wreq.get (toString url)

getExtension :: Text -> Text
getExtension =
  toString
    <&> FilePath.takeExtension
    <&> toText
    <&> Text.dropAround (== '.')

collectEntries :: Text -> Entry -> IO [(Feed.Entry, UTCTime)]
collectEntries url entry
  | Text.isSuffixOf "/" entry.link = do
      entries <- fetchIndex path
      join <$> forM entries (collectEntries path)
  | ext `notElem` hiddenTypes = pure [(feedEntry, entry.time)]
  | otherwise = pure []
 where
  path = url <> entry.link
  ext = getExtension entry.link
  feedEntry =
    ( Feed.nullEntry
        (Text.dropAround (== '/') entry.link)
        (Feed.TextString entry.title)
        (timestamp entry.time)
    )
      { Feed.entryLinks = [Feed.nullLink path]
      }

hiddenTypes :: [Text]
hiddenTypes = ["txt", "srt", "sub", "idx"]

data FeedInfo = MkFeedInfo
  { path :: Text
  , title :: Text
  , size :: Int
  , time :: UTCTime
  }

{- | Scrape an nginx fancy index.
 | Create one RSS feed for every subfolder of the given folder.
-}
main :: IO ()
main = do
  [root_dir_str] <- getArgs
  let root_dir = [i|#{root_dir_str}/|]
  folders <- fetchIndex root_dir
  feeds <- forM folders \entry -> do
    let path = Text.dropAround (== '/') entry.link
    entries <- collectEntries root_dir entry

    -- The Timestamp reported from nginx fancy index does not report changes in subsubfolders.
    -- So we calculate it ourselves.
    let time = List.maximum (entry.time : fmap snd entries)
        emptyFeed =
          Feed.nullFeed
            [i|#{path}-#{timestamp time}|]
            (Feed.TextString entry.title)
            (timestamp time)
        feed =
          fromMaybe (error "Could not produce feed.") $
            Feed.textFeed
              emptyFeed
                { Feed.feedEntries = fmap fst entries
                , Feed.feedLinks = [Feed.nullLink (root_dir <> entry.link)]
                }
    writeFileLText (toString entry.title <.> "xml") feed
    pure
      MkFeedInfo
        { path = path <> ".xml"
        , title = entry.title
        , size = length entries
        , time
        }
  writeFileText [i|index.html|] (mkFeedIndex feeds)

mkFeedIndex :: [FeedInfo] -> Text
mkFeedIndex = \feeds ->
  Text.unlines $
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
    , "<title>Available RSS Feeds</title>"
    , "</head>"
    , "<body>"
    , "<h1>Available RSS Feeds</h1>"
    , "<i>"
    , "Report generate by the"
    , "< a href =\"https://git.maralorn.de/nixos-config/tree/packages/rssfeeds/Folders.hs\">"
    , "folders2rss"
    , "</a>"
    , "script."
    , "</i>"
    , "<ul>"
    ]
      ++ concatMap
        ( \feedInfo ->
            [ "<li>"
            , makeFeedLink feedInfo
            , "</li>"
            ]
        )
        feeds
      ++ [ "</ul>"
         , "</body>"
         , "</html>"
         ]

makeFeedLink :: FeedInfo -> Text
makeFeedLink = \MkFeedInfo{..} ->
  [i|a href="#{path}">#{title} (Entries: #{size} Newest: #{timestamp time})</a>|]

timestamp :: UTCTime -> Text
timestamp = into . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M"
