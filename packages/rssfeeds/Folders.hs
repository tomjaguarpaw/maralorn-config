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
import Text.Atom.Feed qualified as Feed
import Text.Atom.Feed.Export qualified as Feed
import Text.HTML.TagSoup qualified as TagSoup
import Witch (into)

extractItem :: [TagSoup.Tag Text] -> Maybe (Text, Text, UTCTime)
extractItem x = case ( List.firstJust extractFolder
                        . filter (TagSoup.isTagOpenName "a")
                        $ x
                     , TagSoup.maybeTagText
                        <=< ( find TagSoup.isTagText
                                . dropWhile
                                  (\tag -> not (TagSoup.isTagOpenName "td" tag && "date" == TagSoup.fromAttrib "class" tag))
                            )
                        $ x
                     ) of
  (Just (link, title), Just date_str)
    | Just date <- Time.parseTimeM True Time.defaultTimeLocale "%Y-%b-%d %H:%M" (toString date_str) ->
        Just (link, title, date)
  _ -> Nothing

extractIndex :: Text -> IO [(Text, Text, UTCTime)]
extractIndex = \url ->
  mapMaybe extractItem
    . List.split (TagSoup.isTagOpenName "tr")
    . dropWhile (not . TagSoup.isTagOpenName "tbody")
    . TagSoup.parseTags
    . decodeUtf8
    . (^. Wreq.responseBody)
    <$> Wreq.get [i|#{url}|]

collectEntries :: Text -> (Text, Text, UTCTime) -> IO [(Feed.Entry, UTCTime)]
collectEntries = \url (link, title, date) ->
  if Text.isSuffixOf "/" link
    then do
      entries <- extractIndex [i|#{url}#{link}|]
      join <$> forM entries (collectEntries [i|#{url}#{link}|])
    else do
      let ext = Text.dropAround (== '.') . toText . FilePath.takeExtension . toString $ link
      pure
        [ ( ( Feed.nullEntry
                (Text.dropAround (== '/') link)
                (Feed.TextString title)
                (timestamp date)
            )
              { Feed.entryLinks = [Feed.nullLink [i|#{url}#{link}|]]
              }
          , date
          )
        | ext `notElem` hiddenTypes
        ]

-- Scrape an nginx fancy index.
-- Create one RSS feed for every subfolder of the given folder.

hiddenTypes :: [Text]
hiddenTypes = ["txt", "srt", "sub", "idx"]

main :: IO ()
main = do
  [root_dir] <- getArgs
  folders <- extractIndex (toText root_dir)
  feeds <- forM folders \x@(link, title, date') -> do
    let path = Text.dropAround (== '/') link
    entries <- collectEntries [i|#{root_dir}/|] x
    let date = List.maximum (date' : fmap snd entries)
        emptyFeed =
          Feed.nullFeed
            [i|#{path}-#{timestamp date}|]
            (Feed.TextString title)
            (timestamp date)
        feed =
          fromMaybe (error "Could not produce feed.") $
            Feed.textFeed
              emptyFeed
                { Feed.feedEntries = fmap fst entries
                , Feed.feedLinks = [Feed.nullLink [i|#{root_dir}/#{link}|]]
                }
    writeFileLText [i|#{title}.xml|] feed
    pure [i|<li><a href="#{path}.xml">#{title} (#{length entries} entries, newest from #{timestamp date})</a></li>|]
  writeFileText [i|index.html|] $
    Text.unlines $
      [ "<!DOCTYPE html>"
      , "<html>"
      , "<head>"
      , "<title>Available RSS Feeds</title>"
      , "</head>"
      , "<body>"
      , "<h1>Available RSS Feeds</h1>"
      , "<ul>"
      ]
        ++ feeds
        ++ [ "</ul>"
           , "</body>"
           , "</html>"
           ]

timestamp :: UTCTime -> Text
timestamp = into . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M"

extractFolder :: TagSoup.Tag Text -> Maybe (Text, Text)
extractFolder = \tag ->
  let
    title = TagSoup.fromAttrib "title" tag
    href = TagSoup.fromAttrib "href" tag
   in
    if Text.null title || Text.null href
      then Nothing
      else Just (href, title)
