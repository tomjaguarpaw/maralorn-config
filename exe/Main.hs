module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Yaml (FromJSON)
import qualified Data.Yaml as Yaml
import Database.Persist ((==.))
import qualified Database.Persist as Persist
import Database.Persist.Sqlite (runMigration, runSqlite)
import qualified Database.Persist.TH as Persist
import Network.Matrix.Client (ClientSession, Event (..), MatrixIO, MatrixToken (MatrixToken), MessageTextType (..), RoomID (RoomID), RoomMessage (..), TxnID (..), createSession, getJoinedRooms, joinRoom)
import qualified Network.Matrix.Client as Matrix
import Relude
import qualified System.Process.Typed as Process
import System.Random (randomRIO)

data MatrixConfig = MatrixConfig
  { token :: Text
  , server :: Text
  , room :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Config = Config
  { matrix :: MatrixConfig
  , database :: Text
  , repo :: FilePath
  , branches :: Map Text [Text]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

Persist.share
  [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"]
  [Persist.persistLowerCase|
  Branch
    name Text
    commit Text
    deriving Eq Show
    Primary name
  PrSubscription
    user Text
    pullRequest Int
    Primary user pullRequest
    deriving Eq Show
  UserQuery
    user Text
    query Text
    deriving Eq Show
    Primary user
  SessionState
    key Text
    value Text
    Primary key
|]

data Commit = Commit
  { commitId :: Text
  , commitTitle :: Text
  }
  deriving stock (Show)

ensureJoin :: ClientSession -> RoomID -> IO ()
ensureJoin session roomId = do
  rooms <- unwrapMatrixError $ getJoinedRooms session
  unless (roomId `elem` rooms) $ void . unwrapMatrixError $ joinRoom session (coerce roomId)

unwrapMatrixError :: MatrixIO a -> IO a
unwrapMatrixError action = do
  message_response <- action
  case message_response of
    Left matrix_error -> do
      print matrix_error
      exitFailure
    Right response -> pure response

git :: Config -> [String] -> Process.ProcessConfig () () ()
git config command = Process.proc "git" ("-C" : repo config : command)

gitShow :: Config -> [String] -> IO [Commit]
gitShow config reference = do
  raw_commits <- Process.readProcessStdout_ $ git config $ "show" : "-s" : "--format=format:%H %s" : reference
  pure $ uncurry Commit . second (Text.drop 1) . Text.breakOn " " <$> lines (decodeUtf8 raw_commits)

commitIsPR :: Commit -> Maybe Int
commitIsPR commit = do
  (githubId, _) <- second (Text.drop 1) . Text.breakOn " " <$> Text.stripPrefix "Merge pull request #" (commitTitle commit)
  readMaybe (toString githubId)

originBranch :: Text -> String
originBranch = ("origin/" <>) . toString

baseUrl :: Text
baseUrl = "https://github.com/NixOS/nixpkgs/"

commitHTML :: Commit -> MessageText
commitHTML Commit{commitId, commitTitle} = repoLink ("commit/" <> commitId) (Text.take 7 commitId <> " " <> commitTitle)

mention :: Text -> MessageText
mention user = link ("https://matrix.to/#/" <> user) user

link :: Text -> Text -> MessageText
link url label = (label, "<a href=\"" <> url <> "\">" <> label <> "</a>")

repoLink :: Text -> Text -> MessageText
repoLink url = link (baseUrl <> url)

branchHTML :: Text -> MessageText
branchHTML branch = repoLink ("tree/" <> branch) branch

prHTML :: (Int, [Text]) -> MessageText
prHTML (pr, subscribers) = repoLink ("pull/" <> show pr) ("#" <> show pr) <> if null subscribers then m "" else m " (" <> intercalateMsgPlain ", " (fmap mention subscribers) <> m ")"

type MessageText = (Text, Text)

intercalateMsg :: Text -> Text -> [MessageText] -> MessageText
intercalateMsg plain html msgs = (Text.intercalate plain (fmap fst msgs), Text.intercalate html (fmap snd msgs))

unlinesMsg :: [MessageText] -> MessageText
unlinesMsg = intercalateMsg "\n" "<br>"

intercalateMsgPlain :: Text -> [MessageText] -> MessageText
intercalateMsgPlain x = intercalateMsg x x

m :: Text -> (Text, Text)
m x = (x, x)

watchRepo :: Config -> Matrix.ClientSession -> IO ()
watchRepo config matrix_session = do
  Process.runProcess_ (git config $ "fetch" : "-q" : "origin" : fmap toString (Map.keys $ branches config))
  messages <-
    catMaybes <$> runSqlite (database config) do
      forM (Map.toList $ branches config) \(branch, ignores) -> do
        [commit] <- lift $ lift $ lift $ gitShow config [originBranch branch]
        let key = BranchKey branch
        branchState :: Maybe Branch <- Persist.get key
        Persist.repsert key $ Branch branch (commitId commit)
        case branchState of
          Just (Branch{branchCommit}) -> do
            changes <- lift $ lift $ lift $ gitShow config $ [toString $ branchCommit <> "..." <> commitId commit, "--not"] <> fmap originBranch ignores
            prs <- forM (mapMaybe commitIsPR changes) \pr -> do
              subscriptions <- Persist.selectList [PrSubscriptionPullRequest ==. pr] []
              pure (pr, fmap (prSubscriptionUser . Persist.entityVal) subscriptions)
            pure $
              if null changes
                then Nothing
                else
                  Just $
                    unlinesMsg $
                      ( branchHTML branch
                          <> m " advanced by "
                          <> repoLink ("compare/" <> branchCommit <> "..." <> commitId commit) (show (length changes) <> " commits")
                          <> m " to "
                          <> commitHTML commit
                      ) :
                        [m ("Including the " <> show (length prs) <> " PRs: ") <> intercalateMsgPlain ", " (fmap prHTML prs) | not $ null prs]
          Nothing -> do
            putTextLn $ "Initiated database for branch " <> branch <> " at " <> fst (commitHTML commit)
            pure Nothing
  let message = unlinesMsg messages
  if null messages
    then putTextLn "No advances!"
    else sendMessage matrix_session (RoomID . room $ matrix config) message

sendMessage :: ClientSession -> Matrix.RoomID -> MessageText -> IO ()
sendMessage session roomId@(RoomID roomIdText) message = do
  putTextLn $ "Sending to " <> roomIdText <> ": " <> fst message
  txnId <- TxnID . show <$> randomRIO (1000000 :: Int, 9999999)
  void $
    unwrapMatrixError $
      Matrix.sendMessage
        session
        roomId
        (EventRoomMessage $ RoomMessageText $ Matrix.MessageText (fst message) NoticeType (Just "org.matrix.custom.html") (Just (snd message)))
        txnId

sendMessageToUser :: Config -> ClientSession -> Text -> MessageText -> IO ()
sendMessageToUser config session user message = runSqlite (database config) do
  userQuery <- Persist.get (UserQueryKey user)
  case userQuery of
    Just queryPair -> lift . lift . lift $ sendMessage session (RoomID $ userQueryQuery queryPair) message
    Nothing -> putTextLn $ "Not finding a query for " <> user <> " can‘t send message: " <> fst message

data Command = MkCommand
  { roomId :: Matrix.RoomID
  , command :: Text
  , args :: Text
  , author :: Text
  , isQuery :: Bool
  }

getCommands :: ClientSession -> Matrix.RoomID -> NonEmpty Matrix.RoomEvent -> IO [Command]
getCommands session roomId events = do
  maybe [] toList <$> forM (nonEmpty $ mapMaybe getCommand $ toList events) \messages -> do
    members <- unwrapMatrixError $ Matrix.getRoomMembers session roomId
    let isQuery = Map.size members <= 2
    forM (toList messages) \(author, message) -> do
      let (command, args) = second (Text.drop 1) $ Text.breakOn " " message
      pure $ MkCommand{command, args, author, isQuery, roomId}
 where
  getCommand Matrix.RoomEvent{Matrix.reSender = Matrix.Author author, Matrix.reContent = EventRoomMessage (RoomMessageText (Matrix.MessageText{Matrix.mtBody, Matrix.mtType = Matrix.TextType}))} = Just (author, mtBody)
  getCommand _ = Nothing

joinInvites :: ClientSession -> Maybe Matrix.SyncResultRoom -> IO ()
joinInvites session (Just (Matrix.SyncResultRoom{Matrix.srrInvite = Just invites})) =
  forM_ (Map.keys invites) (unwrapMatrixError . Matrix.joinRoom session)
joinInvites _ _ = pass

saveNextBatch :: Config -> Text -> IO ()
saveNextBatch config next_batch = runSqlite (database config) $ Persist.repsert (SessionStateKey' "next_batch") (SessionState "next_batch" next_batch)

setQueries :: Config -> [Command] -> IO ()
setQueries config commands = runSqlite (database config) do
  Persist.repsertMany . fmap (\command -> (UserQueryKey (author command), UserQuery (author command) (coerce $ roomId command))) . filter isQuery $ commands

resultHandler :: Config -> ClientSession -> Matrix.SyncResult -> IO ()
resultHandler config session syncResult@Matrix.SyncResult{Matrix.srNextBatch, Matrix.srRooms} = do
  saveNextBatch config srNextBatch
  joinInvites session srRooms
  commands <- join <$> mapM (uncurry (getCommands session)) (Matrix.getTimelines syncResult)
  setQueries config commands
  forM_ (filter isQuery commands) \case
    MkCommand{command = "subscribe", author, args} ->
      case readMaybe (toString args) :: Maybe Int of
        Nothing -> sendMessageToUser config session author $ m $ "Could not parse \"" <> args <> "\" as a pr number."
        Just number -> runSqlite (database config) do
          existingSubscription <- Persist.selectList [PrSubscriptionUser ==. author, PrSubscriptionPullRequest ==. number] []
          if null existingSubscription
            then do
              Persist.delete $ PrSubscriptionKey author number
              lift . lift . lift $ sendMessageToUser config session author $ m "You are now subscribed to pr " <> prHTML (number, mempty)
            else lift . lift . lift $ sendMessageToUser config session author $ m "You were already subscribed to pr " <> prHTML (number, mempty)
    MkCommand{command = "unsubscribe", author, args} ->
      case readMaybe (toString args) :: Maybe Int of
        Nothing -> sendMessageToUser config session author $ m $ "Could not parse \"" <> args <> "\" as a pr number."
        Just number -> runSqlite (database config) do
          existingSubscription <- Persist.selectList [PrSubscriptionUser ==. author, PrSubscriptionPullRequest ==. number] []
          if null existingSubscription
            then do
              Persist.insert_ $ PrSubscription author number
              lift . lift . lift $ sendMessageToUser config session author $ m "You are now unsubscribed from pr " <> prHTML (number, mempty)
            else lift . lift . lift $ sendMessageToUser config session author $ m "You were already unsubscribed from pr " <> prHTML (number, mempty)
    MkCommand{command = "subscriptions", author} -> runSqlite (database config) do
      existingSubscriptions <- Persist.selectList [PrSubscriptionUser ==. author] []
      if null existingSubscriptions
        then do
          lift . lift . lift $ sendMessageToUser config session author $ m "You are not subscribed to any prs."
        else lift . lift . lift $ sendMessageToUser config session author $ unlinesMsg (m ("You are subscribed to these " <> show (length existingSubscriptions) <> " prs:") : fmap (prHTML . (,mempty) . prSubscriptionPullRequest . Persist.entityVal) existingSubscriptions)
    MkCommand{command = "help", author} ->
      sendMessageToUser config session author $
        unlinesMsg
          [ m "Available commands:"
          , m "help: This command"
          , m "subscribe <pr-number>: Subscribe to a PR."
          , m "unsubscribe <pr-number>: Unsubscribe from a PR."
          , m "subscriptions: List PRs you are subscribed to."
          ]
    MkCommand{command, author} -> sendMessageToUser config session author (unlinesMsg [m $ "Unknown command: " <> command, m "Try the \"help\" command."])

-- forM_ subscriptions $
--  runSqlite (database config) . mapM_ \(author, prId) ->

main :: IO ()
main = do
  [config_path] <- getArgs
  config :: Config <- Yaml.decodeFileThrow config_path
  first_next_batch <- runSqlite (database config) do
    runMigration migrateAll
    fmap sessionStateValue <$> Persist.get (SessionStateKey' "next_batch")
  matrix_session <- createSession (server $ matrix config) (MatrixToken . token $ matrix config)
  userId <- unwrapMatrixError $ Matrix.getTokenOwner matrix_session
  filterId <- unwrapMatrixError $ Matrix.createFilter matrix_session userId Matrix.messageFilter
  let roomId = RoomID . room $ matrix config
  ensureJoin matrix_session roomId
  let keepWatching = watchRepo config matrix_session >> threadDelay 60000000 >> keepWatching
      keepListening = Matrix.syncPoll matrix_session (Just filterId) first_next_batch (Just Matrix.Online) (resultHandler config matrix_session)
  Async.concurrently_ keepWatching keepListening
