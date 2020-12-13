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
{-# LANGUAGE DuplicateRecordFields #-}

import           Prelude                        ( )
import           Relude
import qualified Notmuch
import           Say
import           Data.String.Interpolate
import qualified Data.MIME                     as MIME
import           Data.MIME.Charset
import           Control.Lens            hiding ( argument )
import           Control.Error                  ( withExceptT
                                                , throwE
                                                , tryJust
                                                )
import qualified Data.Text                     as T
import           Control.Monad.Catch            ( MonadCatch
                                                , handleIOError
                                                )
import           Data.Time
import           Relude.Extra.Group
import qualified Data.Map                      as Map
import qualified Options.Applicative           as O
import           Text.Atom.Feed.Export          ( textFeed )
import           Text.Atom.Feed

data Options = Options
  { dbPath :: String
  , folder :: String
  }

data Thread = Thread
  { subject    :: Text
  , threadid   :: ByteString
  , authors    :: [Text]
  , date       :: UTCTime
  , totalCount :: Int
  , messages   :: [Message]
  }
type Error = Text

data Body = HTMLBody Text | TextBody Text

data Message = Message
  { date   :: UTCTime
  , header :: [(Text, Text)]
  , body   :: Body
  }

main :: IO ()
main = do
  Options { dbPath, folder } <- O.execParser $ O.info
    (    Options
    <$>  O.argument
           O.str
           (  O.metavar "DBPATH"
           <> O.help "The full path to the notmuch database"
           )
    <*>  O.argument
           O.str
           (O.metavar "FOLDER" <> O.help "The maildir to scan for messages.")
    <**> O.helper
    )
    O.fullDesc
  res <- runExceptT do
    (thrds, msgs) <- withExceptT
      (\(er :: Notmuch.Status) ->
        [i|Failed to read notmuch data.\ndb path: #{dbPath}\nquery: Folder #{folder}\nerror: #{er}|]
      )
      do
        db <- Notmuch.databaseOpenReadOnly dbPath
        q  <- Notmuch.query db (Notmuch.Folder folder)
        (,) <$> Notmuch.threads q <*> Notmuch.messages q
    msgsByThread  <- forM msgs \msg -> Notmuch.threadId msg <&> (, Right msg)
    thrdsByThread <- forM thrds \thrd -> Notmuch.threadId thrd <&> (, Left thrd)
    result        <-
      mapM (runExceptT . processThread) . Map.toList $ fmap snd <$> groupBy
        fst
        (msgsByThread <> thrdsByThread)
    now <- lift getCurrentTime
    let entries = threadToEntry <$> sortOn (date :: Thread -> UTCTime) (rights result)
        feed    = nullFeed [i|read-later-e-mails-#{timestamp now}|]
                           (TextString "Readlater-E-Mail")
                           (timestamp now)
        errors = lefts result
    feedText <-
      tryJust [i|Failed to generate feed.|] . textFeed $ feed
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

threadToEntry :: Thread -> Entry
threadToEntry Thread { subject, messages, threadid, totalCount, date, authors } =
  (nullEntry threadUrl threadTitle (timestamp date))
    { entryContent = Just . HTMLContent $ content,
      entryAuthors = (\x -> nullPerson { personName = x }) <$> authors
    }
 where
  threadUrl  = [i|thread-#{threadid}-#{timestamp date}|]
  threadTitle = TextString [i|#{subject} (#{length messages}/#{totalCount})|]
  content     = T.intercalate [i|<br>\n<hr>\n|] (messageToHtml <$> messages)

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
  :: (MonadIO m, MonadCatch m)
  => ( Notmuch.ThreadId
     , NonEmpty (Either (Notmuch.Thread a) (Notmuch.Message n a))
     )
  -> ExceptT Error m Thread
processThread (threadid, toList -> thrdAndMsgs) =
  handleIOError (\er -> throwE [i|IOError: #{er}|]) $ do
    thread <-
      tryJust [i|No Thread object found for Threadid #{threadid}|]
      . viaNonEmpty head
      . lefts
      $ thrdAndMsgs
    let msgs = rights thrdAndMsgs
    results <- mapM processMessage msgs
    let messages = sortOn (date :: Message -> UTCTime) results
    subject    <- decodeUtf8 <$> Notmuch.threadSubject thread
    totalCount <- Notmuch.threadTotalMessages thread
    authors <- (^. Notmuch.matchedAuthors) <$> Notmuch.threadAuthors thread
    date <- Notmuch.threadNewestDate thread
    pure (Thread { subject, threadid, messages, totalCount, authors, date })


messageToHtml :: Message -> Text
messageToHtml Message { header, body } = T.intercalate "<br>\n" $ ((\(name, content) -> [i|<b>#{name}:</b> #{content}|]) <$> header) <> ["<br>\n", bodyToHtml body]

bodyToHtml :: Body -> Text
bodyToHtml (HTMLBody x) = x
bodyToHtml (TextBody x) = T.intercalate "<br>\n" . T.splitOn "\n" $ x

processMessage :: (MonadIO m, MonadCatch m) => Notmuch.Message n a -> m Message
processMessage msg = do
  fileName  <- Notmuch.messageFilename msg
  date      <- Notmuch.messageDate msg
  subject   <- tryHdr "subject" msg
  fromField <- tryHdr "from" msg
  toField   <- tryHdr "to" msg
  cc        <- tryHdr "cc" msg
  let hdrs = mapMaybe
        (\(x, a) -> (x, ) <$> a)
        [ ("Subject", subject)
        , ("From"   , fromField)
        , ("To"     , toField)
        , ("Cc"     , cc)
        , ("Date"   , Just (timestamp date))
        ]
  msgEither <- runExceptT $ withExceptT
    (\er -> [i|Failed to read msg\nFilename:#{fileName}\nerror: #{er}|])
    do
      msgContent <- handleIOError (\er -> throwE [i|IOError: #{er}|])
        $ readFileBS fileName
      parseResult <- hoistEither . first toText $ MIME.parse
        (MIME.message MIME.mime)
        msgContent
      textPart <- tryJust [i|No text or html part in message|] $ firstOf
        (MIME.entities . filtered isHtml <> MIME.entities . filtered isTextPlain
        )
        parseResult
      (if isHtml textPart then HTMLBody else TextBody)
        <$> tryJust [i|Could not decode message|] (decode textPart)
  pure $ Message { date, header = hdrs, body = either TextBody id msgEither }

tryHdr :: MonadIO m => ByteString -> Notmuch.Message n a -> m (Maybe Text)
tryHdr h msg =
  ((\x -> if x /= "" then Just x else Nothing) . decodeUtf8 =<<)
    <$> Notmuch.messageHeader h msg

isTextPlain :: MIME.WireEntity -> Bool
isTextPlain =
  MIME.matchContentType "text" (Just "plain") . view MIME.contentType

isHtml :: MIME.WireEntity -> Bool
isHtml = MIME.matchContentType "text" (Just "html") . view MIME.contentType

decode :: MIME.WireEntity -> Maybe Text
decode = preview
  (MIME.transferDecoded' . _Right . charsetText' defaultCharsets . _Right)
