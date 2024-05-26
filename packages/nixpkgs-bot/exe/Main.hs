{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception qualified as Exception
import Control.Monad.Catch (MonadMask)
import Control.Monad.Catch qualified as MonadCatch
import Control.Monad.Except qualified as Except
import Control.Monad.Logger qualified as MonadLogger
import Control.Monad.Trans.Resource qualified as ResourceT
import Data.GraphQL (get)
import Data.GraphQL qualified as GraphQL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Yaml (FromJSON)
import Data.Yaml qualified as Yaml
import Database.Esqueleto.Experimental (notIn, (&&.), (<.), (==.), (^.))
import Database.Esqueleto.Experimental qualified as SQL
import Database.Persist qualified as Persist
import Database.Persist.Sqlite qualified as Persist.Sqlite
import Database.Persist.TH qualified as Persist
import Network.HTTP.Client qualified as HTTP
import Network.Matrix.Client qualified as Matrix
import NixpkgsBot.GraphQL.API qualified as GraphQL.API
import Relude hiding (get)
import System.Clock qualified as Clock
import System.Environment qualified as System
import System.Process.Typed qualified as Process
import System.Random qualified as Random

data Repo = MkRepo
  { localPath :: FilePath
  , owner :: Text
  , name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Config = MkConfig
  { server :: Text
  , database :: Text
  , repo :: Repo
  , branches :: Map Text [Text]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

loadCredential :: String -> IO Text
loadCredential name = do
  credentials_directory <- System.getEnv "CREDENTIALS_DIRECTORY"
  Text.strip . decodeUtf8 <$> readFileBS (credentials_directory <> "/" <> name)

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
  Invite
    room Text
    at Time.UTCTime
    tries Int
    deriving Eq Show
    Primary room
  AuthorSubscription
    user Text
    githubLogin Text
    deriving Eq Show
    Primary user githubLogin
  |]

data Commit = Commit
  { commitId :: Text
  , commitTitle :: Text
  }
  deriving stock (Show, Eq)

getEnv :: (Environment -> a) -> App a
getEnv getter = lift $ lift $ lift $ asks getter

instance Exception Matrix.MatrixError

instance Exception Text

unwrapMatrixErrorT :: (MonadMask m, MonadIO m) => Matrix.MatrixM m a -> m a
unwrapMatrixErrorT action = do
  message_response <- Matrix.retry action
  case message_response of
    Left matrix_error -> do
      liftIO $ Exception.throwIO matrix_error
    Right response -> pure response

unwrapMatrixError :: MonadIO m => Matrix.MatrixIO a -> m a
unwrapMatrixError = liftIO . unwrapMatrixErrorT

git :: Config -> [String] -> Process.ProcessConfig () () ()
git config command = Process.proc "git" ("-C" : localPath (repo config) : command)

gitShow :: [String] -> App [Commit]
gitShow reference = do
  conf <- getEnv config
  raw_commits <- Process.readProcessStdout_ $ git conf $ "show" : "-s" : "--format=format:%H %s" : reference
  pure $ uncurry Commit . second (Text.drop 1) . Text.breakOn " " <$> lines (decodeUtf8 raw_commits)

originBranch :: Text -> String
originBranch = ("origin/" <>) . toString

baseUrl :: App Text
baseUrl = do
  owner_ <- getEnv (owner . repo . config)
  name_ <- getEnv (name . repo . config)
  pure $ "https://github.com/" <> owner_ <> "/" <> name_ <> "/"

commitHTML :: Commit -> App MessageText
commitHTML Commit{commitId, commitTitle} = repoLink ("commit/" <> commitId) (Text.take 7 commitId <> " " <> commitTitle)

link :: Text -> Text -> MessageText
link url label = (label, "<a href=\"" <> url <> "\">" <> label <> "</a>")

repoLink :: Text -> Text -> App MessageText
repoLink url label = do
  prefix <- baseUrl
  pure $ link (prefix <> url) label

branchHTML :: Text -> App MessageText
branchHTML branch = repoLink ("tree/" <> branch) branch

prHTML :: PRInfo -> App MessageText
prHTML
  (PullRequest{pullRequestNumber, pullRequestTitle}, branches) = do
    branch_info <- forM branches \(arrived, branch) -> (m (if arrived then "✅ " else "⏳") <>) <$> branchHTML branch
    link_ <-
      repoLink
        ("pull/" <> show pullRequestNumber)
        ("#" <> show pullRequestNumber <> " " <> pullRequestTitle)
    pure $ link_ <> if null branches then m "" else m " " <> intercalateMsgPlain " " branch_info

type MessageText = (Text, Text)

intercalateMsg :: Text -> Text -> [MessageText] -> MessageText
intercalateMsg plain html msgs = (Text.intercalate plain (fmap fst msgs), Text.intercalate html (fmap snd msgs))

intercalateMsgPlain :: Text -> [MessageText] -> MessageText
intercalateMsgPlain x = intercalateMsg x x

unlinesMsg :: [MessageText] -> MessageText
unlinesMsg = intercalateMsg "\n" "<br>"

m :: Text -> MessageText
m x = (x, x)

mention :: Text -> MessageText
mention user = link ("https://matrix.to/#/" <> user) user

data Environment = MkEnvironment
  { config :: Config
  , matrixSession :: Matrix.ClientSession
  , githubToken :: Text
  , lastWatch :: IORef Clock.TimeSpec
  , lastMaintenance :: IORef Clock.TimeSpec
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
  set_arrived branch = void $ Persist.insertUnique_ (Arrival (PullRequestKey (pullRequestNumber pr)) (BranchKey branch))
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
  github_token <- getEnv githubToken
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

type PRSchema = [GraphQL.unwrap| (GraphQL.API.PullRequestSchema).repository!.pullRequest! |]

type RateLimitSchema = [GraphQL.unwrap| (GraphQL.API.PullRequestSchema).rateLimit! |]

extractPR :: PRSchema -> App (PullRequest, Maybe Merge, Text)
extractPR pr = do
  when (isNothing [get|pr.mergeCommit|] && [get|pr.merged|]) $
    MonadCatch.throwM ("PR is merged but has no merge commit:" <> show pr :: Text)
  pure
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
    , [get|pr.author!.login|]
    )

getMissingAuthorSubscriptions :: Persist.Key PullRequest -> Text -> App [Text]
getMissingAuthorSubscriptions pr_key author = do
  let users_subscribed_to_this_pr = do
        sub <- SQL.from $ SQL.table @Subscription
        SQL.where_ (sub ^. SubscriptionPullRequest ==. SQL.val pr_key)
        pure (sub ^. SubscriptionUser)
  author_subs <- SQL.select $ do
    author_sub <- SQL.from $ SQL.table @AuthorSubscription
    SQL.where_
      ( author_sub
          ^. AuthorSubscriptionGithubLogin
          ==. SQL.val author
          SQL.&&. author_sub
            ^. AuthorSubscriptionUser
              `notIn` SQL.subSelectList users_subscribed_to_this_pr
      )
    pure author_sub
  pure $ fmap (authorSubscriptionUser . Persist.entityVal) author_subs

checkRateLimit :: RateLimitSchema -> App ()
checkRateLimit rateLimit = when ([get|rateLimit.remaining|] < warn_threshold) $ putTextLn $ show rateLimit
 where
  -- Currently github grants 5000 queries per hour
  -- The threshold should probably be lower, but for debugging purposes, I currently want to see everytime a graphql Query happens.
  warn_threshold = 5000

queryPR :: Persist.Key PullRequest -> App (PullRequest, Maybe Merge, Text)
queryPR (PullRequestKey number) = do
  owner <- getEnv (owner . repo . config)
  name <- getEnv (name . repo . config)
  result <- queryGraphQL GraphQL.API.PullRequestQuery{GraphQL.API._number = number, _owner = owner, _name = name}
  putText $ "PRQuery: " <> show number <> " "
  checkRateLimit [get|result.rateLimit!|]
  extractPR [get|result.repository!.pullRequest!|]

getPRInfo :: Persist.Key PullRequest -> App (Maybe PRInfo)
getPRInfo pr_key = do
  savedPRInfoMaybe <- Persist.get pr_key
  case savedPRInfoMaybe of
    Just savedPRInfo -> do
      branches <- reachedBranches savedPRInfo
      pure $ Just (savedPRInfo, branches)
    Nothing -> do
      prInfo <- catchAll $ queryPR pr_key
      forM prInfo \(pull_request, pull_request_merge, author) -> do
        Persist.insert_ pull_request
        ensureSubscriptions pr_key author =<< getMissingAuthorSubscriptions pr_key author
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

ensureSubscriptions :: Persist.Key PullRequest -> Text -> [Text] -> App ()
ensureSubscriptions pr_key author missing_subscriptions = do
  forM_ missing_subscriptions \user -> void $ Persist.insertUnique_ (Subscription user pr_key)
  pr_msg <- mapM prHTML =<< getPRInfo pr_key
  forM_ pr_msg \msg -> forM_ missing_subscriptions \user ->
    sendMessageToUser user $
      m ("Because you are following user " <> author <> " I subscribed you to the pull request ")
        <> msg

{- | This function
 | 1. Looks if a commit is a known merge commit, and marks as arrived.
 | 2. Sees if the commit itself looks like a merge commit for a known PR, creates a merge entry, marks as arrived and asks github if bases don‘t match.
 | 3. Asks github, if we are currently looking for unmerged PRs and the commit doesn‘t seem to belong to any known PRs, then it marks as arrived, if known
 | 4. If any of the before happened for a subscribed PR, return the subscriptions.
-}
findSubscribedPRsInCommitList :: Text -> [Commit] -> [Commit] -> App [Persist.Key PullRequest]
findSubscribedPRsInCommitList branch possible_new_merge_commits =
  fmap join . mapM \change ->
    either id id <$> runExceptT do
      found_by_merge_commit <-
        lift $
          fmap (unMergeKey . Persist.entityKey)
            <$> SQL.select do
              merge <- SQL.from $ SQL.table @Merge
              SQL.where_ (merge ^. MergeCommit ==. SQL.val (commitId change))
              pure merge
      unless (null found_by_merge_commit) (Except.throwError found_by_merge_commit)
      -- Branches which beginn with "nix" are channel branches and we don‘t scan those for new merge_commits.
      when (Text.isPrefixOf "nix" branch) $ Except.throwError []
      unless (change `elem` possible_new_merge_commits) $ Except.throwError []
      lift do
        owner <- getEnv (owner . repo . config)
        name <- getEnv (name . repo . config)
        result <-
          queryGraphQL
            GraphQL.API.MergingPullRequestQuery{GraphQL.API._commit = commitId change, _m_owner = owner, _m_name = name}
        putText $ "MergingQuery: " <> show change <> " "
        checkRateLimit [get|result.rateLimit!|]
        prs <- mapM extractPR $ catMaybes [get|result.repository!.object!.__fragment!.associatedPullRequests!.nodes!|]
        flip mapMaybeM prs \(pr, merge, author) ->
          case merge of
            Just mergeProof | mergeCommit mergeProof == commitId change -> do
              let pr_key = mergeNumber mergeProof
              missing_subs <- getMissingAuthorSubscriptions pr_key author
              pr_is_watched <- isJust <$> Persist.get (mergeNumber mergeProof)
              if pr_is_watched || not (null missing_subs)
                then do
                  Persist.repsert pr_key pr
                  Persist.repsert (MergeKey pr_key) mergeProof
                  ensureSubscriptions pr_key author missing_subs
                  pure $ Just pr_key
                else pure Nothing
            _ -> pure Nothing

notifySubscribers :: Text -> [Persist.Key PullRequest] -> App [Subscription]
notifySubscribers branch =
  fmap join . mapM \pr -> do
    subscriptions <-
      fmap Persist.entityVal <$> SQL.select do
        sub <- SQL.from $ SQL.table @Subscription
        SQL.where_ (sub ^. SubscriptionPullRequest ==. SQL.val pr)
        pure sub
    unless (null subscriptions) $ void $ Persist.insertUnique_ (Arrival pr (BranchKey branch))
    pure subscriptions

watchRepo :: App ()
watchRepo = do
  conf <- getEnv config
  Process.runProcess_ (git conf $ "fetch" : "-q" : "origin" : fmap toString (Map.keys $ branches conf))
  messages <-
    catMaybes <$> do
      forM (Map.toList $ branches conf) \(branch, next) -> do
        [new_commit] <- gitShow [originBranch branch]
        let key = BranchKey branch
        old_branch_state :: Maybe Branch <- Persist.get key
        Persist.repsert key $ Branch branch (commitId new_commit)
        case old_branch_state of
          Just (Branch{branchCommit = old_commit}) -> do
            let diffQuery = [toString $ old_commit <> "..." <> commitId new_commit, "--not"] <> fmap originBranch next
            -- This is number of new commits the github UI will show and the user will expect to see.
            changes <- gitShow diffQuery
            -- These are the commits, that are actually direct successors of
            -- the previous branch head and therefor the only ones that can be merge commits.
            possible_new_merge_requests <- gitShow $ "--ancestry-path" : diffQuery
            prs <- notifySubscribers branch =<< findSubscribedPRsInCommitList branch possible_new_merge_requests changes
            if null changes
              then pure Nothing
              else
                Just
                  . (,prs)
                  . fold
                  <$> sequence
                    [ branchHTML branch
                    , pure $ m " advanced by "
                    , repoLink ("compare/" <> old_commit <> "..." <> commitId new_commit) (show (length changes) <> " commits")
                    , pure $ m " to "
                    , commitHTML new_commit
                    ]
          Nothing -> do
            putTextLn . (("Initiated database for branch " <> branch <> " at ") <>) . fst =<< commitHTML new_commit
            pure Nothing
  forM_ messages $ \(msg, subscriptions) -> forM_ (NonEmpty.groupAllWith subscriptionUser subscriptions) \subscriptionsByUser -> do
    let author = subscriptionUser $ head subscriptionsByUser
        prs = toList $ subscriptionPullRequest <$> subscriptionsByUser
    prMsgs <- sequence =<< mapMaybeM (fmap (fmap prHTML) . getPRInfo) prs
    sendMessageToUser author $
      unlinesMsg $
        msg
          : m ("Including these " <> show (length prMsgs) <> " pull requests you subscribed to:")
          : prMsgs
  unsubscribeFromFinishedPRs
  -- Run some maintenance daily
  whenTimeIsUp lastMaintenance (60 * 60 * 24) maintenance

maintenance :: App ()
maintenance = do
  dropUnsubscribedPRs
  dropOldBranches
  deleteUnusedQueries
  leaveEmptyRooms

dropOldBranches :: App ()
dropOldBranches = do
  branches <- getEnv (Map.keys . branches . config)
  SQL.delete do
    branch <- SQL.from $ SQL.table @Branch
    SQL.where_ (branch ^. BranchName `notIn` SQL.valList branches)

dropUnsubscribedPRs :: App ()
dropUnsubscribedPRs = do
  SQL.delete do
    pr <- SQL.from $ SQL.table @PullRequest
    SQL.where_ $
      (pr ^. PullRequestId)
        `notIn` SQL.subList_select do
          sub <- SQL.from $ SQL.table @Subscription
          pure (sub ^. SubscriptionPullRequest)

unsubscribeFromFinishedPRs :: App ()
unsubscribeFromFinishedPRs = do
  prs <- SQL.select $ SQL.from $ SQL.table @PullRequest
  forM_ prs $ \pr -> do
    all_reached <- all fst <$> reachedBranches (SQL.entityVal pr)
    when all_reached do
      subs <- SQL.select do
        sub <- SQL.from $ SQL.table @Subscription
        SQL.where_ (sub ^. SubscriptionPullRequest ==. SQL.val (SQL.entityKey pr))
        pure sub
      forM_ subs \sub -> do
        pr_html <- prHTML (SQL.entityVal pr, [])
        sendMessageToUser (subscriptionUser (Persist.entityVal sub)) $
          m "Your subscription of pr "
            <> pr_html
            <> m " has ended, because it reached all relevant branches."
      SQL.delete do
        pr_ <- SQL.from $ SQL.table @PullRequest
        SQL.where_ (pr_ ^. PullRequestId ==. SQL.val (SQL.entityKey pr))

deleteUnusedQueries :: App ()
deleteUnusedQueries = SQL.delete do
  query <- SQL.from $ SQL.table @Query
  SQL.where_ $
    (query ^. QueryUser)
      `notIn` SQL.subList_select do
        sub <- SQL.from $ SQL.table @Subscription
        pure (sub ^. SubscriptionUser)
      &&. (query ^. QueryUser)
        `notIn` SQL.subList_select do
          sub <- SQL.from $ SQL.table @AuthorSubscription
          pure (sub ^. AuthorSubscriptionUser)

leaveEmptyRooms :: App ()
leaveEmptyRooms = do
  session <- getEnv matrixSession
  rooms <- unwrapMatrixError (Matrix.getJoinedRooms session)
  forM_ rooms \room ->
    whenM ((== 1) . length <$> unwrapMatrixError (Matrix.getRoomMembers session room)) $
      unwrapMatrixError $
        Matrix.leaveRoomById session room

sendMessage :: Matrix.RoomID -> MessageText -> App ()
sendMessage roomId@(Matrix.RoomID roomIdText) message = do
  putTextLn $ "in room" <> roomIdText <> ":\n" <> fst message
  txnId <- Matrix.TxnID . show <$> Random.randomRIO (1000000 :: Int, 9999999)
  session <- getEnv matrixSession
  void $
    unwrapMatrixError $
      Matrix.sendMessage
        session
        roomId
        ( Matrix.EventRoomMessage $
            Matrix.RoomMessageText $
              Matrix.MessageText (fst message) Matrix.NoticeType (Just "org.matrix.custom.html") (Just (snd message))
        )
        txnId

sendMessageToUser :: Text -> MessageText -> App ()
sendMessageToUser user message = do
  userQuery <- Persist.get (QueryKey user)
  case userQuery of
    Just queryPair -> do
      putText $ "To user " <> user <> " "
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
    join <$> forM (toList messages) \(author, message) -> forM (Text.lines message) \line -> do
      let (cmd, args) = second (Text.drop 1) $ Text.breakOn " " $ Text.strip line
      pure $ MkCommand{command = Text.toLower cmd, args, author, isQuery, roomId}
 where
  getCommand
    Matrix.RoomEvent
      { Matrix.reSender = Matrix.Author author
      , Matrix.reContent =
        Matrix.EventRoomMessage (Matrix.RoomMessageText (Matrix.MessageText{Matrix.mtBody, Matrix.mtType = Matrix.TextType}))
      } = Just (author, mtBody)
  getCommand _ = Nothing

recordInvites :: Maybe Matrix.SyncResultRoom -> App ()
recordInvites (Just (Matrix.SyncResultRoom{Matrix.srrInvite = Just invites})) = do
  now <- liftIO Time.getCurrentTime
  forM_ (Map.keys invites) \room -> SQL.insert $ Invite room now 0
recordInvites _ = pass

joinInvites :: App ()
joinInvites = do
  session <- getEnv matrixSession
  now <- liftIO Time.getCurrentTime
  dueInvites <-
    fmap SQL.entityVal <$> SQL.select do
      invite <- SQL.from $ SQL.table @Invite
      SQL.where_ (invite ^. InviteAt <. SQL.val now)
      pure invite
  forM_ dueInvites \Invite{inviteRoom, inviteAt, inviteTries} -> do
    joinAttempt <- liftIO $ Matrix.joinRoom session inviteRoom
    let delete = Persist.delete (InviteKey inviteRoom)
        retry_in = toEnum (2 ^ inviteTries)
        num_retries = 16
    case joinAttempt of
      Right _ -> do
        delete
        let roomId = Matrix.RoomID inviteRoom
        members <- unwrapMatrixError $ Matrix.getRoomMembers session roomId
        let isQuery = Map.size members <= 2
        when isQuery $ sendMessage roomId =<< helpMessage
      -- less than roughly a day
      Left _ | inviteTries < num_retries -> do
        putTextLn $ "Failed to join room " <> inviteRoom <> " retrying in " <> show retry_in <> " seconds."
        Persist.repsert (InviteKey inviteRoom) (Invite inviteRoom (Time.addUTCTime retry_in inviteAt) (inviteTries + 1))
      Left _ -> do
        putTextLn $ "Failed to join room " <> inviteRoom <> " " <> show num_retries <> " times. Giving up."
        delete

saveNextBatch :: Text -> App ()
saveNextBatch next_batch = Persist.repsert (SessionStateKey' "next_batch") (SessionState "next_batch" next_batch)

setQueries :: [Command] -> App ()
setQueries commands = do
  forM_ (filter isQuery commands) \command -> do
    current_query <- Persist.get (QueryKey (author command))
    let set_room = void $ Persist.repsert (QueryKey (author command)) (Query (author command) (coerce $ roomId command))
    case current_query of
      Just query
        | queryRoom query == coerce (roomId command) -> pass
        | otherwise -> do
            set_room
            sendMessageToUser (author command) $
              m "Because you sent your most recent message to this room, I will use this room for direct messages to you from now on."
      _ -> do
        putTextLn $ "Setting Query for user " <> author command <> " to " <> coerce (roomId command)
        set_room

parsePRNumber :: Text -> Maybe Int
parsePRNumber = readMaybe . toString . Text.dropWhile (`notElem` ['0' .. '9'])

resultHandler :: Matrix.SyncResult -> App ()
resultHandler syncResult@Matrix.SyncResult{Matrix.srNextBatch, Matrix.srRooms} = do
  saveNextBatch srNextBatch
  recordInvites srRooms
  joinInvites
  commands <- join <$> mapM (uncurry getCommands) (Matrix.getTimelines syncResult)
  setQueries commands
  let hasSub author pr_key =
        null <$> SQL.select do
          sub <- SQL.from $ SQL.table @Subscription
          SQL.where_ (sub ^. SubscriptionUser ==. SQL.val author &&. sub ^. SubscriptionPullRequest ==. SQL.val pr_key)
          pure sub
      hasAuthorSub user author =
        null <$> SQL.select do
          sub <- SQL.from $ SQL.table @AuthorSubscription
          SQL.where_ (sub ^. AuthorSubscriptionUser ==. SQL.val user &&. sub ^. AuthorSubscriptionGithubLogin ==. SQL.val author)
          pure sub
  responses <- forM (filter isQuery commands) \cmd ->
    (cmd,) <$> catchAll case cmd of
      MkCommand{command, author, args}
        | Text.isPrefixOf command "subscribe"
        , split_args <- Text.words args
        , fromMaybe False (viaNonEmpty (flip Text.isPrefixOf "user" . head) split_args) -> do
            case maybeAt 1 split_args of
              Nothing -> pure $ m "Please tell me a user to subscribe to."
              Just user -> do
                notSubbed <- hasAuthorSub author user
                if notSubbed
                  then do
                    void $ Persist.insertUnique_ $ AuthorSubscription author user
                    pure $ m $ "I will now track for you all pull requests by " <> user
                  else pure $ m $ "Okay, but you were already subscribed to pull requests by user " <> user
      MkCommand{command, author, args}
        | Text.isPrefixOf command "unsubscribe"
        , split_args <- Text.words args
        , fromMaybe False (viaNonEmpty (flip Text.isPrefixOf "user" . head) split_args) -> do
            case maybeAt 1 split_args of
              Nothing -> pure $ m "Please tell me a user to unsubscribe from."
              Just user -> do
                notSubbed <- hasAuthorSub author user
                if notSubbed
                  then pure $ m $ "I haven‘t been tracking pull requests by " <> user <> " for you."
                  else do
                    SQL.delete $ do
                      author_sub <- SQL.from $ SQL.table @AuthorSubscription
                      SQL.where_
                        ( author_sub ^. AuthorSubscriptionUser ==. SQL.val author &&. author_sub ^. AuthorSubscriptionGithubLogin ==. SQL.val user
                        )
                    pure $ m $ "I will not subscribe you automatically to new pull requests by user " <> user <> " anymore."
      MkCommand{command, author, args} | Text.isPrefixOf command "subscribe" ->
        case parsePRNumber args of
          Nothing -> pure $ m $ "I could not parse \"" <> args <> "\" as a pull request number. Have you maybe mistyped it?"
          Just number -> do
            let pr_key = PullRequestKey number
            pr_msg_may <- mapM prHTML =<< getPRInfo pr_key
            notSubbed <- hasSub author pr_key
            case pr_msg_may of
              Just prMsg | notSubbed -> do
                void $ Persist.insertUnique_ $ Subscription author pr_key
                pure $ m "I will now track for you the pull request " <> prMsg
              Just prMsg -> pure $ m "Okay, but you were already subscribed to pull request " <> prMsg
              Nothing -> pure $ m $ "Can‘t find information about a pull request with number #" <> show number <> "."
      MkCommand{command, author, args} | Text.isPrefixOf command "unsubscribe" ->
        case parsePRNumber args of
          Nothing -> pure $ m $ "I could not parse \"" <> args <> "\" as a pull request number. Have you maybe mistyped it?"
          Just number -> do
            let pr_key = PullRequestKey number
            pr_msg_may <- mapM prHTML =<< getPRInfo pr_key
            notSubbed <- hasSub author pr_key
            case pr_msg_may of
              Just prMsg
                | notSubbed ->
                    pure $ m "Well, you were not subscribed to pull request " <> prMsg
              Just prMsg -> do
                Persist.delete $ SubscriptionKey author pr_key
                pure $ m "Okay, I will not send you updates about pull request " <> prMsg
              Nothing -> pure $ m $ "Can‘t find information about a pull request with number #" <> show number <> "."
      MkCommand{command, author} | Text.isPrefixOf command "list" -> do
        existingSubscriptions <- SQL.select $ do
          sub <- SQL.from $ SQL.table @Subscription
          SQL.where_ (sub ^. SubscriptionUser ==. SQL.val author)
          pure sub
        existingAuthorSubscriptions <- SQL.select $ do
          sub <- SQL.from $ SQL.table @AuthorSubscription
          SQL.where_ (sub ^. AuthorSubscriptionUser ==. SQL.val author)
          pure sub
        if null existingSubscriptions && null existingAuthorSubscriptions
          then do
            pure $
              m "I am currently not tracking any pull requests or users for you. Use "
                <> codeHTML "subscribe"
                <> m " to change that."
          else do
            prMsgs <-
              sequence
                =<< mapMaybeM (fmap (fmap prHTML) . getPRInfo . subscriptionPullRequest . Persist.entityVal) existingSubscriptions
            pure $
              unlinesMsg
                ( m ("I am currently watching the following " <> show (length existingSubscriptions) <> " pull requests for you:")
                    : prMsgs
                      <> [ m
                            ( "Also I am subscribing you to new pull requests by the users: "
                                <> Text.intercalate ", " (fmap (authorSubscriptionGithubLogin . Persist.entityVal) existingAuthorSubscriptions)
                            )
                         ]
                )
      MkCommand{command} | Text.isPrefixOf command "help" -> helpMessage
      MkCommand{command} ->
        pure
          ( unlinesMsg
              [ m "Sorry, I don‘t know what you want from me, when your command starts with: " <> codeHTML command
              , m "I‘ll tell you all commands I know, when you use the " <> codeHTML "help" <> m " command."
              ]
          )
  forM_ responses $ uncurry \MkCommand{author} -> sendMessageToUser author . fromMaybe (m "Your command triggered an internal error in the bot. Sorry about that.")
  whenTimeIsUp lastWatch 60 watchRepo

helpMessage :: App MessageText
helpMessage = do
  branchList <- join $ getEnv (fmap (intercalateMsgPlain ", ") . mapM branchHTML . Map.keys . branches . config)
  repo_link <- repoLink "" "nixpkgs git repository on github"
  pure $
    unlinesMsg
      [ m
          "Hey! I am the friendly nixpkgs-bot and I am here to help you notice when pull requests are being merged, so you don‘t need to hammer refresh on github."
      , mempty
      , m "I am continously watching the "
          <> repo_link
          <> m
            ". If you want to be notified whenever a PR reaches one of the relevant branches in the nixpkgs release cycle, you can tell me via the following commands:"
      , mempty
      , codeHTML "subscribe [pr-number]" <> m ": I will subscribe you to the given pull request."
      , codeHTML "unsubscribe [pr-number]" <> m ": I will unsubscribe you from the given pull request."
      , codeHTML "subscribe user [github-login-name]"
          <> m ": I will subscribe you to all newly merged pull requests created by the given github handle."
      , codeHTML "unsubscribe user [github-login-name]"
          <> m ": I will not subscribe you to newly merged pull requests from the given user anymore."
      , codeHTML "list" <> m ": I will show you all the pull requests, I am watching for you."
      , codeHTML "help" <> m ": So I can tell you all of this again."
      , mempty
      , m "By the way, you don‘t need to type the whole command, for every keyword in the command any prefix will work. e.g. "
          <> codeHTML "s u"
          <> m " for "
          <> codeHTML "subscribe user"
      , mempty
      , m "I will inform you, when one of the pull requests you subscribed to reaches one of these branches: " <> branchList
      , mempty
      , m "I have been programmed and am being hosted by "
          <> mention "@maralorn:maralorn.de"
          <> m ". Feel free to reach out to him, if you have any problems or suggestions."
      , m "My code is written in Haskell, is open source under the AGPL license and can be found at "
          <> link
            "https://code.maralorn.de/maralorn/config/src/branch/main/packages/nixpkgs-bot"
            "code.maralorn.de/maralorn/config/src/branch/main/packages/nixpkgs-bot"
          <> m "."
      ]

whenTimeIsUp :: (Environment -> IORef Clock.TimeSpec) -> Int64 -> App () -> App ()
whenTimeIsUp get_ref interval action = do
  last_run_ref <- getEnv get_ref
  last_run <- readIORef last_run_ref
  now <- liftIO $ Clock.getTime Clock.Monotonic
  when (Clock.sec (Clock.diffTimeSpec now last_run) > interval) do
    writeIORef last_run_ref now
    action

codeHTML :: (Semigroup b, IsString b) => b -> (b, b)
codeHTML label = (label, "<code>" <> label <> "</code>")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  [config_path] <- getArgs
  config <- Yaml.decodeFileThrow config_path
  matrix_token <- loadCredential "matrix_token"
  matrix_session <- Matrix.createSession (server config) (Matrix.MatrixToken matrix_token)
  github_token <- loadCredential "github_token"
  now <- liftIO $ Clock.getTime Clock.Monotonic
  last_watch <- newIORef now
  last_maintenance <- newIORef minBound
  let runApp :: App a -> IO a
      runApp =
        flip runReaderT (MkEnvironment config matrix_session github_token last_watch last_maintenance)
          . Persist.Sqlite.runSqlite (database config)
          . Persist.Sqlite.retryOnBusy
  first_next_batch <- runApp do
    Persist.Sqlite.runMigration migrateAll
    watchRepo
    fmap sessionStateValue <$> Persist.get (SessionStateKey' "next_batch")
  userId <- unwrapMatrixError $ Matrix.getTokenOwner matrix_session
  filterId <- unwrapMatrixError $ Matrix.createFilter matrix_session userId Matrix.messageFilter
  unwrapMatrixError $
    Matrix.syncPoll
      matrix_session
      (Just filterId)
      first_next_batch
      (Just Matrix.Online)
      (void . catchAll . runApp . resultHandler)

catchAll :: (MonadIO m, MonadCatch.MonadCatch m) => m a -> m (Maybe a)
catchAll action =
  MonadCatch.catch
    (Just <$> action)
    (\(e :: SomeException) -> putStrLn ("### ERROR ###: " <> displayException e) >> pure Nothing)
