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
import           Control.Error                  ( withExceptT )
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

type Thread = Text
type Error = Text
type MyMessage = (UTCTime, Text)

main :: IO ()
main = do
  Options { dbPath, folder } <- execParser $ info
    (   Options
    <$> argument
          str
          (metavar "DBPATH" <> help "The full path to the notmuch database")
    <*> argument
          str
          (metavar "FOLDER" <> help "The maildir to scan for messages.")
    <**> helper)
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
    let entries = threadToEntry $ fst <$> rights result
        feed    = nullFeed [i|mail-threads-#{timestamp now}|]
                           (TextString "Read-Later-Mail")
                           (timestamp now)
    forM_ (rights result) (say . fst)
    forM_ (rights result) (mapM_ sayErr . snd)
    forM_ (lefts result)  sayErr

    feedText <- hoistEither
        .   maybeToRight [i|Failed to generate feed.|] . textFeed $ feed { feedEntries = entries }
    say $ toStrict feedText
  either
    (\(er :: Text) ->
      sayErr [i|mail2feed failed to export mails to rss.\n#{er}|]
    )
    (const pass)
    res

threadToEntry :: [Thread] -> [Entry]
threadToEntry = error "not implemented"

errorsToEntry :: [Error] -> Entry
errorsToEntry er = undefined

timestamp :: UTCTime -> Text
timestamp = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

processThread
  :: (MonadIO m)
  => (ThreadId, NonEmpty (Either (Notmuch.Thread a) (Notmuch.Message n a)))
  -> ExceptT Error m (Thread, [Error])
processThread (thrdId, toList -> thrdAndMsgs) = do
  thread <-
    hoistEither
    . maybeToRight [i|No Thread object found for Threadid #{thrdId}|]
    . viaNonEmpty head
    . lefts
    $ thrdAndMsgs
  subject  <- threadSubject thread
  msgCount <- threadTotalMessages thread
  let
    msgs = rights thrdAndMsgs
    threadHeader =
      [i|Showing #{length msgs} of #{msgCount} e-mails from thread\nSubject: #{subject}\n\n|] :: Text
  results <- mapM (runExceptT . processMessage) msgs
  let goodResults = snd <$> sortOn fst (rights results)
  pure
    ([i|#{threadHeader}\n#{T.intercalate "\n\n" goodResults}|], lefts results)

processMessage
  :: (MonadIO m) => Notmuch.Message n a -> ExceptT Error m MyMessage
processMessage msg = do
  fileName <- messageFilename msg
  withExceptT
    (\er -> [i|Failed to read msg #{fileName}\nerror: #{er}|])
    do
      date    <- messageDate msg
      subject <-
        hoistEither
        .   maybeToRight [i|Failed to get subject|]
        =<< messageHeader "subject" msg
      fromField <-
        hoistEither
        .   maybeToRight [i|Failed to get from|]
        =<< messageHeader "from" msg
      toField <-
        hoistEither
        .   maybeToRight [i|Failed to get to|]
        =<< messageHeader "to" msg
      cc <-
        hoistEither
        .   maybeToRight [i|Failed to get cc|]
        =<< messageHeader "cc" msg
      msgContent <- withExceptT (\(er :: IOException) -> [i|IOError: #{er}|])
        $ readFileBS fileName
      parseResult <- hoistEither . first toText $ parse (message mime)
                                                        msgContent
      textPart <-
        hoistEither . maybeToRight [i|No text part in message|] $ firstOf
          (entities . filtered isTextPlain)
          parseResult
      textAsText <-
        hoistEither . maybeToRight [i|Could not decode message|] $ decode
          textPart
      pure
        ( date
        , [i|Subject: #{subject}\nFrom: #{fromField}\nTo: #{toField}#{if cc /= "" then "\nCc: " <> cc else ""}\nDate: #{date}\n\n#{textAsText}|]
        )

isTextPlain :: WireEntity -> Bool
isTextPlain = matchContentType "text" (Just "plain") . view contentType

decode :: WireEntity -> Maybe Text
decode =
  preview (transferDecoded' . _Right . charsetText' defaultCharsets . _Right)
