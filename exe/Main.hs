module Main (main) where

import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as ResourceT
import Data.GraphQL (get)
import qualified Data.GraphQL as GraphQL
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.Yaml (FromJSON)
import qualified Data.Yaml as Yaml
import Database.Persist ((==.))
import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Persist.Sqlite
import qualified Database.Persist.TH as Persist
import qualified Network.HTTP.Client as HTTP
import Network.Matrix.Client (Event (..), MatrixToken (MatrixToken), MessageTextType (..), RoomID (RoomID), RoomMessage (..), TxnID (..))
import qualified Network.Matrix.Client as Matrix
import NixpkgsBot.GraphQL.API (PullRequestQuery (PullRequestQuery), _number)
import Relude hiding (get)
import qualified System.Clock as Clock
import qualified System.Process.Typed as Process
import qualified System.Random as Random

data MatrixConfig = MatrixConfig
  { token :: Text
  , server :: Text
  , room :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Config = Config
  { matrix :: MatrixConfig
  , githubToken :: Text
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
    deriving Eq Show
    Primary key
  PullRequest
    number Int
    title Text
    author Text
    baseBranch Text
    deriving Eq Show
    Primary number
  PullRequestMerge
    number Int
    at Time.UTCTime
    commit Text
    by Text
    deriving Eq Show
    Primary number
|]

data Commit = Commit
  { commitId :: Text
  , commitTitle :: Text
  }
  deriving stock (Show)

ensureJoin :: RoomID -> App ()
ensureJoin roomId = do
  session <- getEnv matrixSession
  rooms <- unwrapMatrixError $ Matrix.getJoinedRooms session
  unless (roomId `elem` rooms) $ void . unwrapMatrixError $ Matrix.joinRoom session (coerce roomId)

getEnv :: (Environment -> a) -> App a
getEnv getter = lift $ lift $ lift $ asks getter

unwrapMatrixErrorT :: MonadIO m => Matrix.MatrixM m a -> m a
unwrapMatrixErrorT action = do
  message_response <- action
  case message_response of
    Left matrix_error -> do
      print matrix_error
      exitFailure
    Right response -> pure response

unwrapMatrixError :: MonadIO m => Matrix.MatrixIO a -> m a
unwrapMatrixError = liftIO . unwrapMatrixErrorT

git :: Config -> [String] -> Process.ProcessConfig () () ()
git config command = Process.proc "git" ("-C" : repo config : command)

gitShow :: [String] -> App [Commit]
gitShow reference = do
  conf <- getEnv config
  raw_commits <- Process.readProcessStdout_ $ git conf $ "show" : "-s" : "--format=format:%H %s" : reference
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

link :: Text -> Text -> MessageText
link url label = (label, "<a href=\"" <> url <> "\">" <> label <> "</a>")

repoLink :: Text -> Text -> MessageText
repoLink url = link (baseUrl <> url)

branchHTML :: Text -> MessageText
branchHTML branch = repoLink ("tree/" <> branch) branch

prHTML :: PRInfo -> MessageText
prHTML
  (PullRequest{pullRequestNumber, pullRequestTitle}, _, branches) =
    repoLink
      ("pull/" <> show pullRequestNumber)
      ("#" <> show pullRequestNumber <> " " <> pullRequestTitle)
      <> if null branches then mempty else m " contained in " <> intercalateMsgPlain ", " (fmap branchHTML branches)

type MessageText = (Text, Text)

intercalateMsg :: Text -> Text -> [MessageText] -> MessageText
intercalateMsg plain html msgs = (Text.intercalate plain (fmap fst msgs), Text.intercalate html (fmap snd msgs))

intercalateMsgPlain :: Text -> [MessageText] -> MessageText
intercalateMsgPlain x = intercalateMsg x x

unlinesMsg :: [MessageText] -> MessageText
unlinesMsg = intercalateMsg "\n" "<br>"

m :: Text -> MessageText
m x = (x, x)

data Environment = MkEnvironment
  { config :: Config
  , matrixSession :: Matrix.ClientSession
  , lastWatch :: IORef Clock.TimeSpec
  }

type App a = ReaderT Persist.Sqlite.SqlBackend (MonadLogger.NoLoggingT (ResourceT.ResourceT (ReaderT Environment IO))) a

type PRInfo = (PullRequest, Maybe PullRequestMerge, [Text])

findBranches :: Text -> App [Text]
findBranches merge_commit = do
  conf <- getEnv config
  flip mapMaybeM (Map.keys $ branches conf) \branch -> do
    commitsWithout <- gitShow [toString merge_commit, "--not", originBranch branch, "-1"]
    pure $ if null commitsWithout then Just branch else Nothing

getPRInfo :: Int -> App PRInfo
getPRInfo number = do
  savedPRInfoMaybe <- Persist.get (PullRequestKey number)
  savedMergeInfo <- Persist.get (PullRequestMergeKey number)
  case savedPRInfoMaybe of
    Just savedPRInfo -> do
      branches <- maybe (pure []) (findBranches . pullRequestMergeCommit) savedMergeInfo
      pure (savedPRInfo, savedMergeInfo, branches)
    Nothing -> do
      github_token <- getEnv (githubToken . config)
      result <-
        GraphQL.runGraphQLQueryT
          GraphQL.defaultGraphQLSettings
            { GraphQL.url = "https://api.github.com/graphql"
            , GraphQL.modifyReq = \r ->
                r
                  { HTTP.requestHeaders =
                      HTTP.requestHeaders r
                        <> [ ("User-Agent", "nixpkgs-bot")
                           , ("Authorization", "Bearer " <> encodeUtf8 github_token)
                           ]
                  }
            }
          $ GraphQL.runQuery PullRequestQuery{_number = number}
      let pr = [get|result.repository!.pullRequest!|]
          pull_request =
            PullRequest
              { pullRequestNumber = number
              , pullRequestTitle = [get|pr.title|]
              , pullRequestAuthor = [get|pr.author!.login|]
              , pullRequestBaseBranch = [get|pr.baseRefName|]
              }
          pull_request_merge =
            [get|pr.mergedAt|] <&> \mergedAt ->
              PullRequestMerge
                { pullRequestMergeNumber = number
                , pullRequestMergeAt = mergedAt
                , pullRequestMergeBy = [get|pr.mergedBy!.login|]
                , pullRequestMergeCommit = [get|pr.mergeCommit!.oid|]
                }
      Persist.insert_ pull_request
      inBranches <- case pull_request_merge of
        Just merge_info -> do
          Persist.insert_ merge_info
          findBranches (pullRequestMergeCommit merge_info)
        Nothing -> pure []
      pure
        ( pull_request
        , pull_request_merge
        , inBranches
        )

watchRepo :: App ()
watchRepo = do
  conf <- getEnv config
  Process.runProcess_ (git conf $ "fetch" : "-q" : "origin" : fmap toString (Map.keys $ branches conf))
  messages <-
    catMaybes <$> do
      forM (Map.toList $ branches conf) \(branch, ignores) -> do
        [commit] <- gitShow [originBranch branch]
        let key = BranchKey branch
        branchState :: Maybe Branch <- Persist.get key
        Persist.repsert key $ Branch branch (commitId commit)
        case branchState of
          Just (Branch{branchCommit}) -> do
            changes <- gitShow $ [toString $ branchCommit <> "..." <> commitId commit, "--not"] <> fmap originBranch ignores
            prs <-
              (toList <=< catMaybes) <$> forM (mapMaybe commitIsPR changes) \pr -> do
                subscriptions <- Persist.selectList [PrSubscriptionPullRequest ==. pr] []
                pure $ nonEmpty $ fmap ((,pr) . prSubscriptionUser . Persist.entityVal) subscriptions
            pure $
              if null changes
                then Nothing
                else
                  Just
                    ( branchHTML branch
                        <> m " advanced by "
                        <> repoLink ("compare/" <> branchCommit <> "..." <> commitId commit) (show (length changes) <> " commits")
                        <> m " to "
                        <> commitHTML commit
                    , prs
                    )
          Nothing -> do
            putTextLn $ "Initiated database for branch " <> branch <> " at " <> fst (commitHTML commit)
            pure Nothing
  if null messages
    then putTextLn "No advances!"
    else do
      let message = unlinesMsg (fst <$> messages)
      sendMessage (RoomID . room $ matrix conf) message
      forM_ messages $ \(msg, prsWithAuthors) -> forM_ (NonEmpty.groupAllWith fst prsWithAuthors) \prsByAuthor -> do
        let author = fst $ head prsByAuthor
            prs = toList $ snd <$> prsByAuthor
        prMsgs <- mapM (fmap prHTML . getPRInfo) prs
        sendMessageToUser author $ unlinesMsg $ msg : m ("Including these " <> show (length prMsgs) <> " pull requests you subscribed to:") : prMsgs

sendMessage :: Matrix.RoomID -> MessageText -> App ()
sendMessage roomId@(RoomID roomIdText) message = do
  putTextLn $ "Sending to " <> roomIdText <> ": " <> fst message
  txnId <- TxnID . show <$> Random.randomRIO (1000000 :: Int, 9999999)
  session <- getEnv matrixSession
  void $
    unwrapMatrixError $
      Matrix.sendMessage
        session
        roomId
        (EventRoomMessage $ RoomMessageText $ Matrix.MessageText (fst message) NoticeType (Just "org.matrix.custom.html") (Just (snd message)))
        txnId

sendMessageToUser :: Text -> MessageText -> App ()
sendMessageToUser user message = do
  userQuery <- Persist.get (UserQueryKey user)
  case userQuery of
    Just queryPair -> sendMessage (RoomID $ userQueryQuery queryPair) message
    Nothing -> putTextLn $ "Not finding a query for " <> user <> " can‘t send message: " <> fst message

data Command = MkCommand
  { roomId :: Matrix.RoomID
  , command :: Text
  , args :: Text
  , author :: Text
  , isQuery :: Bool
  }

getCommands :: Matrix.RoomID -> NonEmpty Matrix.RoomEvent -> App [Command]
getCommands roomId events = do
  maybe [] toList <$> forM (nonEmpty $ mapMaybe getCommand $ toList events) \messages -> do
    session <- getEnv matrixSession

    members <- unwrapMatrixError $ Matrix.getRoomMembers session roomId
    let isQuery = Map.size members <= 2
    forM (toList messages) \(author, message) -> do
      let (command, args) = second (Text.drop 1) $ Text.breakOn " " message
      pure $ MkCommand{command, args, author, isQuery, roomId}
 where
  getCommand Matrix.RoomEvent{Matrix.reSender = Matrix.Author author, Matrix.reContent = EventRoomMessage (RoomMessageText (Matrix.MessageText{Matrix.mtBody, Matrix.mtType = Matrix.TextType}))} = Just (author, mtBody)
  getCommand _ = Nothing

joinInvites :: Maybe Matrix.SyncResultRoom -> App ()
joinInvites (Just (Matrix.SyncResultRoom{Matrix.srrInvite = Just invites})) = do
  session <- getEnv matrixSession
  forM_ (Map.keys invites) (unwrapMatrixError . Matrix.joinRoom session)
joinInvites _ = pass

saveNextBatch :: Text -> App ()
saveNextBatch next_batch = Persist.repsert (SessionStateKey' "next_batch") (SessionState "next_batch" next_batch)

setQueries :: [Command] -> App ()
setQueries commands = do
  Persist.repsertMany . fmap (\command -> (UserQueryKey (author command), UserQuery (author command) (coerce $ roomId command))) . filter isQuery $ commands

resultHandler :: Matrix.SyncResult -> App ()
resultHandler syncResult@Matrix.SyncResult{Matrix.srNextBatch, Matrix.srRooms} = do
  saveNextBatch srNextBatch
  joinInvites srRooms
  commands <- join <$> mapM (uncurry getCommands) (Matrix.getTimelines syncResult)
  setQueries commands
  forM_ (filter isQuery commands) \case
    MkCommand{command = "add", author, args} ->
      case readMaybe (toString args) :: Maybe Int of
        Nothing -> sendMessageToUser author $ m $ "Could not parse \"" <> args <> "\" as a pr number."
        Just number -> do
          existingSubscription <- Persist.selectList [PrSubscriptionUser ==. author, PrSubscriptionPullRequest ==. number] []
          prMsg <- prHTML <$> getPRInfo number
          if null existingSubscription
            then do
              Persist.insert_ $ PrSubscription author number
              sendMessageToUser author $ m "You are now subscribed to pr " <> prMsg
            else sendMessageToUser author $ m "You were already subscribed to pr " <> prMsg
    MkCommand{command = "delete", author, args} ->
      case readMaybe (toString args) :: Maybe Int of
        Nothing -> sendMessageToUser author $ m $ "Could not parse \"" <> args <> "\" as a pr number."
        Just number -> do
          existingSubscription <- Persist.selectList [PrSubscriptionUser ==. author, PrSubscriptionPullRequest ==. number] []
          prMsg <- prHTML <$> getPRInfo number
          if null existingSubscription
            then do
              sendMessageToUser author $ m "You were already unsubscribed from pr " <> prMsg
            else do
              Persist.delete $ PrSubscriptionKey author number
              sendMessageToUser author $ m "You are now unsubscribed from pr " <> prMsg
    MkCommand{command = "list", author} -> do
      existingSubscriptions <- Persist.selectList [PrSubscriptionUser ==. author] []
      if null existingSubscriptions
        then do
          sendMessageToUser author $ m "You are not subscribed to any prs."
        else do
          prMsgs <- mapM (fmap prHTML . getPRInfo . prSubscriptionPullRequest . Persist.entityVal) existingSubscriptions
          sendMessageToUser author $ unlinesMsg (m ("You are subscribed to these " <> show (length existingSubscriptions) <> " prs:") : prMsgs)
    MkCommand{command = "help", author} ->
      sendMessageToUser author $
        unlinesMsg
          [ m "Available commands:"
          , codeHTML "help" <> m ": This command"
          , codeHTML "add [pr-number]" <> m ": Subscribe to a PR."
          , codeHTML "delete [pr-number]" <> m ": Unsubscribe from a PR."
          , codeHTML "list" <> m ": List PRs you are subscribed to."
          ]
    MkCommand{command, author} -> sendMessageToUser author (unlinesMsg [m $ "Unknown command: " <> command, m "Try the " <> codeHTML "help" <> m " command."])
  last_watch_ref <- getEnv lastWatch
  last_watch <- readIORef last_watch_ref
  now <- liftIO $ Clock.getTime Clock.Monotonic
  when (Clock.sec (Clock.diffTimeSpec now last_watch) > 60) do
    writeIORef last_watch_ref now
    watchRepo

codeHTML :: (Semigroup b, IsString b) => b -> (b, b)
codeHTML label = (label, "<code>" <> label <> "</code>")

main :: IO ()
main = do
  [config_path] <- getArgs
  config <- Yaml.decodeFileThrow config_path
  matrix_session <- Matrix.createSession (server $ matrix config) (MatrixToken . token $ matrix config)
  last_watch <- newIORef minBound
  let runApp :: App a -> IO a
      runApp = flip runReaderT (MkEnvironment config matrix_session last_watch) . Persist.Sqlite.runSqlite (database config) . Persist.Sqlite.retryOnBusy
  first_next_batch <- runApp do
    let roomId = RoomID . room $ matrix config
    ensureJoin roomId
    Persist.Sqlite.runMigration migrateAll
    fmap sessionStateValue <$> Persist.get (SessionStateKey' "next_batch")
  userId <- unwrapMatrixError $ Matrix.getTokenOwner matrix_session
  filterId <- unwrapMatrixError $ Matrix.createFilter matrix_session userId Matrix.messageFilter
  unwrapMatrixError $ Matrix.syncPoll matrix_session (Just filterId) first_next_batch (Just Matrix.Online) (runApp . resultHandler)
