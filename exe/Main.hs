module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Yaml (FromJSON)
import qualified Data.Yaml as Yaml
import Database.Persist ((==.))
import qualified Database.Persist as Persist
import Database.Persist.Sqlite (runMigration, runSqlite)
import qualified Database.Persist.TH as Persist
import Network.Matrix.Client (ClientSession, Event (..), MatrixIO, MatrixToken (MatrixToken), MessageTextType (..), RoomID (RoomID), RoomMessage (..), TxnID (..), createSession, getJoinedRooms, joinRoom, sendMessage)
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
    deriving Eq Show
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
commitHTML Commit{commitId, commitTitle} = link ("commit/" <> commitId) (Text.take 7 commitId <> " " <> commitTitle)

link :: Text -> Text -> MessageText
link url label = (label, "<a href=\"" <> baseUrl <> url <> "\">" <> label <> "</a>")

branchHTML :: Text -> MessageText
branchHTML branch = link ("tree/" <> branch) branch

prHTML :: (Int, [Text]) -> MessageText
prHTML (pr, subscribers) = link ("pull/" <> show pr) ("#" <> show pr <> if null subscribers then "" else "(" <> Text.intercalate ", " subscribers <> ")")

type MessageText = (Text, Text)

intercalateMsg :: Text -> Text -> [(Text, Text)] -> (Text, Text)
intercalateMsg plain html msgs = (Text.intercalate plain (fmap fst msgs), Text.intercalate html (fmap snd msgs))

unlinesMsg :: [(Text, Text)] -> (Text, Text)
unlinesMsg = intercalateMsg "\n" "<br>"

intercalateMsgPlain :: Text -> [(Text, Text)] -> (Text, Text)
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
                          <> link ("compare/" <> branchCommit <> "..." <> commitId commit) (show (length changes) <> " commits")
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
    else do
      putTextLn $ "Sending: " <> fst message
      txnId <- TxnID . show <$> randomRIO (1000000 :: Int, 9999999)
      let roomId = RoomID . room $ matrix config
      void $
        unwrapMatrixError $
          sendMessage
            matrix_session
            roomId
            (EventRoomMessage $ RoomMessageText $ Matrix.MessageText (fst message) NoticeType (Just "org.matrix.custom.html") (Just (snd message)))
            txnId

getCommand :: Matrix.RoomEvent -> Maybe (Text, Int)
getCommand Matrix.RoomEvent{Matrix.reSender = Matrix.Author author, Matrix.reContent = EventRoomMessage (RoomMessageText (Matrix.MessageText{Matrix.mtBody, Matrix.mtType = Matrix.TextType}))} =
  do
    subscription <- Text.stripPrefix "!subscribe " mtBody
    prId :: Int <- readMaybe (toString . Text.strip $ subscription)
    pure (author, prId)
getCommand _ = Nothing

main :: IO ()
main = do
  [config_path] <- getArgs
  config :: Config <- Yaml.decodeFileThrow config_path
  runSqlite (database config) (runMigration migrateAll)
  matrix_session <- createSession (server $ matrix config) (MatrixToken . token $ matrix config)
  userId <- unwrapMatrixError $ Matrix.getTokenOwner matrix_session
  filterId <- unwrapMatrixError $ Matrix.createFilter matrix_session userId Matrix.messageFilter
  let roomId = RoomID . room $ matrix config
  ensureJoin matrix_session roomId
  let keepWatching = watchRepo config matrix_session >> threadDelay 60000000 >> keepWatching
      keepListening = Matrix.syncPoll matrix_session (Just filterId) Nothing (Just Matrix.Online) \syncResult -> do
        let events = toList . snd =<< Matrix.getTimelines syncResult
            subscriptions = nonEmpty $ events & mapMaybe getCommand
        forM_ subscriptions $
          runSqlite (database config) . mapM_ \(author, prId) -> do
            existingSubscription <- Persist.selectList [PrSubscriptionUser ==. author, PrSubscriptionPullRequest ==. prId] []
            if null existingSubscription
              then do
                Persist.insert_ $ PrSubscription author prId
                putTextLn $ "Subscribing " <> author <> " to " <> show prId
              else putTextLn $ author <> " is already subscribed to " <> show prId

  Async.concurrently_ keepWatching keepListening
