module Main where

import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Relude hiding (
  intercalate,
  zip,
 )
import System.Environment ()
import System.FilePattern.Directory (getDirectoryFiles)
import Text.Atom.Feed
import Text.Atom.Feed.Export (textFeed)
import Witch

timestamp :: UTCTime -> Text
timestamp = into . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M"

main :: IO ()
main = do
  [out_file, dir] <- getArgs
  now <- Time.getCurrentTime
  new_files <- getDirectoryFiles dir [Time.formatTime Time.defaultTimeLocale "%Y-%m-%d-*/index.html" now]

  let entries =
        fmap
          ( ( \file_name ->
                let
                  folder_name =
                    Text.dropEnd 11 file_name
                 in
                  ( nullEntry
                      folder_name
                      (TextString [i|mastodon digest #{Text.drop 11 folder_name}|])
                      (timestamp now)
                  )
                    { entryLinks = [nullLink [i|http://hera.vpn.m-0.eu:8842/mastodon/#{file_name}|]]
                    }
            )
              . into
          )
          new_files
      feed =
        nullFeed
          [i|mastodon-summary-#{timestamp now}|]
          (TextString "Mastodon Digests")
          (timestamp now)
  whenJust (textFeed feed{feedEntries = entries}) $
    \file -> writeFileLText out_file file
