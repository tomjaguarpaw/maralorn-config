module Main (main) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as ResourceT
import Data.GraphQL (get)
import qualified Data.GraphQL as GraphQL
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Yaml (FromJSON)
import qualified Data.Yaml as Yaml
import Database.Persist ((==.))
import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Persist.Sqlite
import qualified Database.Persist.TH as Persist
import qualified Network.HTTP.Client as HTTP
import qualified Network.Matrix.Client as Matrix
import qualified NixpkgsBot.GraphQL.API as GraphQL.API
import Relude hiding (get)
import qualified System.Clock as Clock
import qualified System.Process.Typed as Process
import qualified System.Random as Random

owner, name :: Text
owner = "NixOS"
name = "nixpkgs"

-- TODO:
-- detect non-merge-commit merges
-- only query those, if necessary
-- notify about changed query
-- drop finished prs
-- be smart about detecting commit progress
-- leave empty rooms

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
  Subscription
    user Text
    pullRequest PullRequestId OnDeleteCascade
    Primary user pullRequest
    deriving Eq Show
  Query
    user Text
    room Text
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
    base Text
    deriving Eq Show
    Primary number
  Merge
    number PullRequestId OnDeleteCascade
    commit Text
    deriving Eq Show
    Primary number
  Arrival
    number PullRequestId OnDeleteCascade
    branch BranchId OnDeleteCascade
    Primary number branch
|]

data Commit = Commit
  { commitId :: Text
  , commitTitle :: Text
  }
  deriving stock (Show)

ensureJoin :: Matrix.RoomID -> App ()
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
baseUrl = "https://github.com/" <> owner <> "/" <> name <> "/"

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
  (PullRequest{pullRequestNumber, pullRequestTitle}, branches) =
    repoLink
      ("pull/" <> show pullRequestNumber)
      ("#" <> show pullRequestNumber <> " " <> pullRequestTitle)
      <> if null branches then m "" else m " " <> intercalateMsgPlain " " (branches <&> \(arrived, branch) -> m (if arrived then "✔ " else "⏳") <> branchHTML branch)

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

type App = ReaderT Persist.Sqlite.SqlBackend (MonadLogger.NoLoggingT (ResourceT.ResourceT (ReaderT Environment IO)))

type PRInfo = (PullRequest, [(Bool, Text)])

reachedBranches :: PullRequest -> App [(Bool, Text)]
reachedBranches pr = hasArrived (pullRequestBase pr)
 where
  hasArrived :: Text -> App [(Bool, Text)]
  hasArrived branch = do
    arrived <- isJust <$> Persist.get (ArrivalKey (PullRequestKey (pullRequestNumber pr)) (BranchKey branch))
    nextBranches <- fromMaybe [] <$> getEnv (Map.lookup branch . branches . config)
    ((arrived, branch) :) . join <$> mapM hasArrived nextBranches

discoverReachedBranches :: PullRequest -> Merge -> App ()
discoverReachedBranches pr merge = do
  set_arrived (pullRequestBase pr)
  check_arrived_in_next (pullRequestBase pr)
 where
  set_arrived branch = Persist.insert_ (Arrival (PullRequestKey (pullRequestNumber pr)) (BranchKey branch))
  check_arrived_in_next :: Text -> App ()
  check_arrived_in_next branch = do
    next_branches <- fromMaybe [] <$> getEnv (Map.lookup branch . branches . config)
    forM_ next_branches check_arrived
  check_arrived :: Text -> App ()
  check_arrived branch = do
    commits_without <- gitShow [toString (mergeCommit merge), "--not", originBranch branch, "-1"]
    when (null commits_without) do
      set_arrived branch
      check_arrived_in_next branch

queryGraphQL :: GraphQL.GraphQLQuery a => a -> App (GraphQL.Object (GraphQL.ResultSchema a))
queryGraphQL query = do
  github_token <- getEnv (githubToken . config)
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
    $ GraphQL.runQuery query

type PRSchema = [GraphQL.unwrap| (GraphQL.API.PullRequestSchema).repository!.pullRequest!|]

extractPR :: PRSchema -> (PullRequest, Maybe Merge)
extractPR pr =
  ( PullRequest
      { pullRequestNumber = [get|pr.number|]
      , pullRequestTitle = [get|pr.title|]
      , pullRequestBase = [get|pr.baseRefName|]
      }
  , [get|pr.mergeCommit|] <&> \commit ->
      Merge
        { mergeNumber = PullRequestKey [get|pr.number|]
        , mergeCommit = [get|commit.oid|]
        }
  )

queryPR :: Persist.Key PullRequest -> App (PullRequest, Maybe Merge)
queryPR (PullRequestKey number) = do
  result <- queryGraphQL GraphQL.API.PullRequestQuery{GraphQL.API._number = number, _owner = owner, _name = name}
  pure $ extractPR [get|result.repository!.pullRequest!|]

getPRInfo :: Persist.Key PullRequest -> App PRInfo
getPRInfo pr_key = do
  savedPRInfoMaybe <- Persist.get pr_key
  case savedPRInfoMaybe of
    Just savedPRInfo -> do
      branches <- reachedBranches savedPRInfo
      pure (savedPRInfo, branches)
    Nothing -> do
      (pull_request, pull_request_merge) <- queryPR pr_key
      Persist.insert_ pull_request
      case pull_request_merge of
        Just merge_info -> do
          Persist.insert_ merge_info
          discoverReachedBranches pull_request merge_info
        Nothing -> pass
      branches <- reachedBranches pull_request
      pure
        ( pull_request
        , branches
        )

{- | This function
 | 1. Looks if a commit is a known merge commit, and marks as arrived.
 | 2. Sees if the commit itself looks like a merge commit for a known PR, creates a merge entry, marks as arrived and asks github if bases don‘t match.
 | 3. Asks github, if we are currently looking for unmerged PRs and the commit doesn‘t seem to belong to any known PRs, then it marks as arrived, if known
 | 4. If any of the before happened for a subscribed PR, return the subscriptions.
-}
detectPRsOnAdvance :: Text -> [Commit] -> App [Subscription]
detectPRsOnAdvance branch =
  withPrs
    <=< fmap join . mapM \change ->
      either id id <$> runExceptT do
        found_by_merge_commit <- lift $ fmap (unMergeKey . Persist.entityKey) <$> Persist.selectList [MergeCommit ==. commitId change] []
        unless (null found_by_merge_commit) (Except.throwError found_by_merge_commit)
        case commitIsPR change of
          Just number -> do
            let pr_key = PullRequestKey number
            prMay <- lift $ Persist.get pr_key -- Are we watching this PR?
            case prMay of
              Just pr -> do
                lift $
                  if pullRequestBase pr /= branch
                    then do
                      (new_pr, merge) <- queryPR pr_key
                      Persist.repsert pr_key new_pr
                      whenJust merge $ Persist.repsert (MergeKey pr_key)
                    else Persist.insert_ $ Merge pr_key (commitId change)
                Except.throwError [pr_key]
              Nothing -> Except.throwError []
          Nothing -> pass
        when (Text.isPrefixOf "nix" branch) $ Except.throwError [] -- Branches which beginn with "nix" are channel branches and we don‘t scan those for new merge_commits.
        result <- lift $ queryGraphQL GraphQL.API.MergingPullRequestQuery{GraphQL.API._commit = commitId change, _owner = owner, _name = name}
        let prs = extractPR <$> catMaybes [get|result.repository!.object!.__fragment!.associatedPullRequests!.nodes!|]
        flip mapMaybeM prs \(pr, merge) ->
          case merge of
            Just mergeProof | mergeCommit mergeProof == commitId change -> lift do
              let pr_key = mergeNumber mergeProof
              pr_is_watched <- isJust <$> Persist.get (mergeNumber mergeProof)
              if pr_is_watched
                then do
                  Persist.repsert pr_key pr
                  Persist.repsert (MergeKey pr_key) mergeProof
                  pure $ Just pr_key
                else pure Nothing
            _ -> pure Nothing
 where
  withPrs :: [Persist.Key PullRequest] -> App [Subscription]
  withPrs =
    fmap join . mapM \pr -> do
      subscriptions <- fmap Persist.entityVal <$> Persist.selectList [SubscriptionPullRequest ==. pr] []
      unless (null subscriptions) $ Persist.insert_ (Arrival pr (BranchKey branch))
      pure subscriptions

watchRepo :: App ()
watchRepo = do
  conf <- getEnv config
  Process.runProcess_ (git conf $ "fetch" : "-q" : "origin" : fmap toString (Map.keys $ branches conf))
  messages <-
    catMaybes <$> do
      forM (Map.toList $ branches conf) \(branch, next) -> do
        [commit] <- gitShow [originBranch branch]
        let key = BranchKey branch
        branchState :: Maybe Branch <- Persist.get key
        Persist.repsert key $ Branch branch (commitId commit)
        case branchState of
          Just (Branch{branchCommit}) -> do
            changes <- gitShow $ [toString $ branchCommit <> "..." <> commitId commit, "--not"] <> fmap originBranch next
            prs <- detectPRsOnAdvance branch changes
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
      sendMessage (Matrix.RoomID . room $ matrix conf) message
      forM_ messages $ \(msg, subscriptions) -> forM_ (NonEmpty.groupAllWith subscriptionUser subscriptions) \subscriptionsByUser -> do
        let author = subscriptionUser $ head subscriptionsByUser
            prs = toList $ subscriptionPullRequest <$> subscriptionsByUser
        prMsgs <- mapM (fmap prHTML . getPRInfo) prs
        sendMessageToUser author $ unlinesMsg $ msg : m ("Including these " <> show (length prMsgs) <> " pull requests you subscribed to:") : prMsgs

sendMessage :: Matrix.RoomID -> MessageText -> App ()
sendMessage roomId@(Matrix.RoomID roomIdText) message = do
  putTextLn $ "Sending to Room" <> roomIdText <> ":\n" <> fst message
  txnId <- Matrix.TxnID . show <$> Random.randomRIO (1000000 :: Int, 9999999)
  session <- getEnv matrixSession
  void $
    unwrapMatrixError $
      Matrix.sendMessage
        session
        roomId
        (Matrix.EventRoomMessage $ Matrix.RoomMessageText $ Matrix.MessageText (fst message) Matrix.NoticeType (Just "org.matrix.custom.html") (Just (snd message)))
        txnId

sendMessageToUser :: Text -> MessageText -> App ()
sendMessageToUser user message = do
  userQuery <- Persist.get (QueryKey user)
  case userQuery of
    Just queryPair -> do
      putTextLn $ "Sending message to user " <> user
      sendMessage (Matrix.RoomID $ queryRoom queryPair) message
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
  getCommand Matrix.RoomEvent{Matrix.reSender = Matrix.Author author, Matrix.reContent = Matrix.EventRoomMessage (Matrix.RoomMessageText (Matrix.MessageText{Matrix.mtBody, Matrix.mtType = Matrix.TextType}))} = Just (author, mtBody)
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
  forM_ (filter isQuery commands) \command -> do
    current_query <- Persist.get (QueryKey (author command))
    case current_query of
      Just query | queryRoom query == coerce (roomId command) -> pass
      _ -> do
        putTextLn $ "Setting Query for user " <> author command <> " from " <> show current_query <> " to " <> coerce (roomId command)
        void $ Persist.repsert (QueryKey (author command)) (Query (author command) (coerce $ roomId command))

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
          let pr_key = PullRequestKey number
          existingSubscription <- Persist.selectList [SubscriptionUser ==. author, SubscriptionPullRequest ==. pr_key] []
          prMsg <- prHTML <$> getPRInfo pr_key
          if null existingSubscription
            then do
              Persist.insert_ $ Subscription author (PullRequestKey number)
              sendMessageToUser author $ m "You are now subscribed to pr " <> prMsg
            else sendMessageToUser author $ m "You were already subscribed to pr " <> prMsg
    MkCommand{command = "delete", author, args} ->
      case readMaybe (toString args) :: Maybe Int of
        Nothing -> sendMessageToUser author $ m $ "Could not parse \"" <> args <> "\" as a pr number."
        Just number -> do
          let pr_key = PullRequestKey number
          existingSubscription <- Persist.selectList [SubscriptionUser ==. author, SubscriptionPullRequest ==. pr_key] []
          prMsg <- prHTML <$> getPRInfo pr_key
          if null existingSubscription
            then do
              sendMessageToUser author $ m "You were already unsubscribed from pr " <> prMsg
            else do
              Persist.delete $ SubscriptionKey author (PullRequestKey number)
              sendMessageToUser author $ m "You are now unsubscribed from pr " <> prMsg
    MkCommand{command = "list", author} -> do
      existingSubscriptions <- Persist.selectList [SubscriptionUser ==. author] []
      if null existingSubscriptions
        then do
          sendMessageToUser author $ m "You are not subscribed to any prs."
        else do
          prMsgs <- mapM (fmap prHTML . getPRInfo . subscriptionPullRequest . Persist.entityVal) existingSubscriptions
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
  matrix_session <- Matrix.createSession (server $ matrix config) (Matrix.MatrixToken . token $ matrix config)
  last_watch <- newIORef minBound
  let runApp :: App a -> IO a
      runApp = flip runReaderT (MkEnvironment config matrix_session last_watch) . Persist.Sqlite.runSqlite (database config) . Persist.Sqlite.retryOnBusy
  first_next_batch <- runApp do
    let roomId = Matrix.RoomID . room $ matrix config
    ensureJoin roomId
    Persist.Sqlite.runMigration migrateAll
    fmap sessionStateValue <$> Persist.get (SessionStateKey' "next_batch")
  userId <- unwrapMatrixError $ Matrix.getTokenOwner matrix_session
  filterId <- unwrapMatrixError $ Matrix.createFilter matrix_session userId Matrix.messageFilter
  unwrapMatrixError $ Matrix.syncPoll matrix_session (Just filterId) first_next_batch (Just Matrix.Online) (runApp . resultHandler)
