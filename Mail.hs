{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
import           Prelude                        ( )
import           Relude
import           Notmuch                 hiding ( Thread )
import qualified Notmuch
import           Say
import           Data.String.Interpolate
import           Data.MIME                     as MIME
import           Data.MIME.Charset
import           Control.Lens            hiding ( argument )
import           Control.Error                  ( tryIO
                                                , withExceptT
                                                )
import qualified Data.Text                     as T
import           Control.Exception
import           Data.Time
import           Relude.Extra.Group
import qualified Data.Map                      as Map
import           Options.Applicative
import           Text.Atom.Feed.Export          ( textFeed )
import           Text.Atom.Feed

data Options = Options
  { dbPath :: String
  , folder :: String
  }

data Thread = Thread
  { subject    :: Text
  , threadid   :: ByteString
  , count      :: Int
  , totalCount :: Int
  , content    :: Text
  }
type Error = Text
type MyMessage = Text

main :: IO ()
main = do
  Options { dbPath, folder } <- execParser $ info
    (    Options
    <$>  argument
           str
           (metavar "DBPATH" <> help "The full path to the notmuch database")
    <*>  argument
           str
           (metavar "FOLDER" <> help "The maildir to scan for messages.")
    <**> helper
    )
    fullDesc
  res <- runExceptT do
    (thrds, msgs) <- withExceptT
      (\(er :: Status) ->
        [i|Failed to read notmuch data.\ndb path: #{dbPath}\nquery: Folder #{folder}\nerror: #{er}|]
      )
      do
        db <- databaseOpenReadOnly dbPath
        q  <- query db (Folder folder)
        (,) <$> threads q <*> messages q
    msgsByThread  <- forM msgs \msg -> threadId msg <&> (, Right msg)
    thrdsByThread <- forM thrds \thrd -> threadId thrd <&> (, Left thrd)
    result        <-
      mapM (runExceptT . processThread) . Map.toList $ fmap snd <$> groupBy
        fst
        (msgsByThread <> thrdsByThread)
    now <- lift getCurrentTime
    let entries = threadToEntry now <$> rights result
        feed    = nullFeed [i|mail-threads-#{timestamp now}|]
                           (TextString "Read-Later-Mail")
                           (timestamp now)
    let errors = lefts result
    feedText <-
      hoistEither . maybeToRight [i|Failed to generate feed.|] . textFeed $ feed
        { feedEntries =
          (if null errors then id else (errorsToEntry now errors :)) entries
        }
    say $ toStrict feedText
  either
    (\(er :: Text) ->
      sayErr [i|mail2feed failed to export mails to rss.\n#{er}|]
    )
    (const pass)
    res

threadToEntry :: UTCTime -> Thread -> Entry
threadToEntry now Thread { subject, content, threadid, count, totalCount } =
  (nullEntry [i|thread-#{threadid}-#{timestamp now}|]
             (TextString [i|#{subject} (#{count}/#{totalCount})|])
             (timestamp now)
    )
    { entryContent = Just
                     . HTMLContent
                     . T.intercalate "<br>\n"
                     . T.splitOn "\n"
                     $ content
    }

errorsToEntry :: UTCTime -> [Error] -> Entry
errorsToEntry now er = (nullEntry [i|mailerrors - #{timestamp now}|]
                                  (TextString [i|Mail processing Errors|])
                                  (timestamp now)
                       )
  { entryContent = Just
                   . HTMLContent
                   . T.intercalate "<br>\n"
                   . T.splitOn "\n"
                   . T.intercalate "\n"
                   $ er
  }

timestamp :: UTCTime -> Text
timestamp = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

processThread
  :: (MonadIO m)
  => (ThreadId, NonEmpty (Either (Notmuch.Thread a) (Notmuch.Message n a)))
  -> ExceptT Error m Thread
processThread (threadid, toList -> thrdAndMsgs) = do
  thread <-
    hoistEither
    . maybeToRight [i|No Thread object found for Threadid #{threadid}|]
    . viaNonEmpty head
    . lefts
    $ thrdAndMsgs
  subject    <- decodeUtf8 <$> threadSubject thread
  totalCount <- threadTotalMessages thread
  let msgs = rights thrdAndMsgs
  results <- mapM processMessage msgs
  let allMsgs = either id id . snd <$> sortOn fst results
      content = T.intercalate [i|\n#{replicate 80 '-'}\n|] allMsgs
  pure (Thread { subject, threadid, content, totalCount, count = length msgs })

processMessage
  :: MonadIO m => Notmuch.Message n a -> m (UTCTime, Either Error MyMessage)
processMessage msg = do
  fileName  <- messageFilename msg
  date      <- messageDate msg
  subject   <- tryHdr "subject" msg
  fromField <- tryHdr "from" msg
  toField   <- tryHdr "to" msg
  cc        <- tryHdr "cc" msg
  let hdrs = fold
        [ "Subject" `hdr` subject
        , "From" `hdr` fromField
        , "To" `hdr` toField
        , "Cc" `hdr` cc
        , [i|Date: #{date}\n|]
        ]
  ((date, ) <$>) . runExceptT $ withExceptT
    (\er -> [i|Failed to read msg\nFilename:#{fileName}\n#{hdrs}error: #{er}|])
    do
      msgContent <-
        withExceptT (\(er :: IOException) -> [i|IOError: #{er}|])
        . tryIO
        $ readFileBS fileName
      parseResult <- hoistEither . first toText $ parse (message mime)
                                                        msgContent
      textPart <-
        hoistEither
        . maybeToRight [i|No text or html part in message|]
        $ firstOf
            (entities . filtered isTextPlain <> entities . filtered isHtml)
            parseResult
      textAsText <-
        hoistEither . maybeToRight [i|Could not decode message|] $ decode
          textPart
      pure [i|#{hdrs}\n#{textAsText}|]

tryHdr :: MonadIO f => ByteString -> Notmuch.Message n a -> f ByteString
tryHdr h = fmap (fromMaybe "") . messageHeader h

hdr :: Text -> ByteString -> Text
hdr _     ""      = ""
hdr label content = [i|#{label}: #{content}\n|]

isTextPlain :: WireEntity -> Bool
isTextPlain = matchContentType "text" (Just "plain") . view contentType

isHtml :: WireEntity -> Bool
isHtml = matchContentType "text" (Just "html") . view contentType

decode :: WireEntity -> Maybe Text
decode =
  preview (transferDecoded' . _Right . charsetText' defaultCharsets . _Right)
