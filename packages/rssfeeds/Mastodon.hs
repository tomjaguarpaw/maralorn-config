module Main (main) where

import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Time qualified as Time
import Relude
import System.FilePattern.Directory qualified as Directory
import Text.Atom.Feed qualified as Feed
import Text.Atom.Feed.Export qualified as Feed
import Witch

timestamp :: Time.ZonedTime -> Text
timestamp = into . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M"

todayMask :: Time.ZonedTime -> Directory.FilePattern
todayMask = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d-*/index.html"

feedFromFileName :: Time.ZonedTime -> Text -> Feed.Entry
feedFromFileName now file_name =
  ( Feed.nullEntry
      folder_name
      (Feed.TextString [i|mastodon digest #{digest_name}|])
      (timestamp now)
  )
    { Feed.entryLinks = [Feed.nullLink [i|http://rss.maralorn.de/own/mastodon/#{file_name}|]]
    }
 where
  digest_name = Text.drop 11 folder_name
  folder_name =
    Text.dropEnd 11 file_name

makeFeed :: Time.ZonedTime -> [FilePath] -> Maybe LazyText.Text
makeFeed now file_names =
  Feed.textFeed emptyFeed{Feed.feedEntries = feedFromFileName now . into <$> file_names}
 where
  emptyFeed =
    Feed.nullFeed
      [i|mastodon-summary-#{timestamp now}|]
      (Feed.TextString "Mastodon Digests")
      (timestamp now)

main :: IO ()
main = do
  [out_file, dir] <- getArgs
  now <- Time.getZonedTime
  file_names <- Directory.getDirectoryFiles dir [todayMask now]
  whenJust (makeFeed now file_names) $
    \file -> writeFileLText out_file file
