module Main (main) where

import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Relude
import Text.Atom.Feed qualified as Feed
import Text.Atom.Feed.Export qualified as Feed
import Witch (into)

timestamp :: UTCTime -> Text
timestamp = into . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M"

mkEntry :: UTCTime -> (Integer, Text) -> Feed.Entry
mkEntry now (index, line) =
  let
    (url : rest) = Text.splitOn "," line
    title' = Text.intercalate "," rest
    title = if Text.null title' then url else title'
   in
    ( Feed.nullEntry
        url
        (Feed.TextString title)
        (timestamp now{Time.utctDayTime = fromInteger (60 * index)})
    )
      { Feed.entryLinks = [Feed.nullLink url]
      }

main :: IO ()
main = do
  [file_path] <- getArgs
  now <- Time.getCurrentTime
  entries <-
    readFileBS file_path
      <&> fmap (mkEntry now)
        . zip [0 ..]
        . lines
        . decodeUtf8
  let
    emptyFeed =
      Feed.nullFeed
        (into file_path)
        (Feed.TextString (toText file_path))
        (timestamp now)
    feed =
      fromMaybe (error "Could not produce feed.") $
        Feed.textFeed
          emptyFeed
            { Feed.feedEntries = entries
            }
  putLTextLn feed
