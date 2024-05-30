module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Aeson hiding (Error, Success)
import Data.Aeson.Optics
import Data.String.Interpolate
import Data.Text qualified as Text
import Network.Wreq
import Network.Wreq qualified as Wreq
import Optics
import Optics.Operators.Unsafe
import Relude
import System.Environment qualified as System

loadCredential :: String -> IO Text
loadCredential name = do
  credentials_directory <- System.getEnv "CREDENTIALS_DIRECTORY"
  Text.strip . decodeUtf8 <$> readFileBS (credentials_directory <> "/" <> name)

data PR = MkPR
  { repo :: Text
  , num :: Integer
  }

data CIState = Pending | Success | Failure | Error deriving stock (Show)

data Commit = MkCommit
  { id :: Text
  , state :: CIState
  , lastUpdate :: Text
  }

data Config = MkConfig
  { wreqOptions :: Wreq.Options
  , forgejoUrl :: Text
  }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  [config_path] <- getArgs
  config_json <- readFileBS config_path
  token <- loadCredential "merge_bot_token"
  let forgejoUrl = config_json ^?! key "server" % _String
      config =
        MkConfig
          { wreqOptions = defaults & lensVL auth ?~ oauth2Bearer (encodeUtf8 token)
          , forgejoUrl
          }
  forever do
    putTextLn [i|Fetching open PRs on #{forgejoUrl} assigned to token owner …|]
    prs <- fetchPRs config
    forM_ prs (processPR config)
    threadDelay 60_000_000

processPR :: Config -> PR -> IO ()
processPR config pr = do
  let num = pr.num
  putTextLn [i|Processing PR \##{num}|]
  stop <- ensureFastForward config pr
  unless stop $ do
    commit <- fetchHeadCommit config pr
    case commit.state of
      Success -> do
        putText "CI success, "
        mergePR config pr
      Pending -> putTextLn "CI pending."
      x -> do
        let msg = [i|CI #{x}.|]
        putTextLn msg
        unlessM (hasRecentComments config pr commit.lastUpdate) $ putComment config pr msg

ensureFastForward :: Config -> PR -> IO Bool
ensureFastForward config pr = do
  resp <-
    postWith
      (config.wreqOptions & lensVL checkResponse ?~ \_ _ -> pure ())
      [i|#{prUrl config pr}/update?style=rebase|]
      (object [])
  case resp ^. lensVL responseStatus % lensVL statusCode of
    x | x < 300 -> do
      putStrLn "Rebase successful."
      pure True
    409 -> do
      putStrLn "Rebase failed."
      pure True
    407 -> do
      putStrLn "Branch is up-to-date."
      pure False
    500 -> do
      putStrLn "Code 500: Assuming branch is up-to-date."
      pure False
    x -> do
      putStrLn [i|Code: #{x}, message #{resp ^? lensVL responseBody % key "message"}|]
      pure True

mergePR :: Config -> PR -> IO ()
mergePR config pr = do
  _ <-
    postWith
      config.wreqOptions
      [i|#{prUrl config pr}/merge|]
      $ object
        [ "Do" .= ("fast-forward-only" :: Text)
        , "delete_branch_after_merge" .= True
        ]
  putTextLn "Merged."

putComment :: Config -> PR -> Text -> IO ()
putComment config pr comment = do
  putTextLn [i|Adding comment: #{comment}|]
  void $ postWith config.wreqOptions [i|#{issueUrl config pr}/comments|] (object ["body" .= comment])

hasRecentComments :: Config -> PR -> Text -> IO Bool
hasRecentComments config pr timestamp = do
  let timestamp' = Text.replace "+" "%2B" timestamp
      num = pr.num
  comments_resp <- getWith config.wreqOptions [i|#{repoUrl config pr }/issues/#{num}/comments?since=#{timestamp'}|]
  pure $ has (lensVL responseBody % values) comments_resp

fetchHeadCommit :: Config -> PR -> IO Commit
fetchHeadCommit config pr = do
  pr_state_resp <- getWith config.wreqOptions [i|#{prUrl config pr}|]
  let commit_id = pr_state_resp ^?! lensVL responseBody % key "head" % key "ref" % _String
  commit_state_resp <- getWith config.wreqOptions [i|#{repoUrl config pr}/commits/#{commit_id}/status|]
  let commit_state_body = commit_state_resp ^. lensVL responseBody
  pure $
    MkCommit
      { id = commit_id
      , state =
          commit_state_body
            ^?! key "state"
            % _String
            % to
              ( \case
                  "" -> Just Pending
                  "success" -> Just Success
                  "pending" -> Just Pending
                  "error" -> Just Error
                  "failure" -> Just Failure
                  _ -> Nothing
              )
            % _Just
      , lastUpdate =
          fromMaybe "updated at timestamp" $
            maximumOf (key "statuses" % values % key "updated_at" % _String) commit_state_body
      }

fetchPRs :: Config -> IO [PR]
fetchPRs config = do
  resp <- getWith config.wreqOptions [i|#{apiUrl config}/repos/issues/search?state=open&type=pulls&assigned=true|]
  pure $
    resp ^.. lensVL responseBody % values % to \pr ->
      MkPR
        { repo = pr ^?! key "repository" % key "full_name" % _String
        , num = pr ^?! key "number" % _Integer
        }

apiUrl :: Config -> Text
apiUrl config = [i|#{url}/api/v1|]
 where
  url = config.forgejoUrl

repoUrl :: Config -> PR -> Text
repoUrl config pr = [i|#{apiUrl config}/repos/#{repo}|]
 where
  repo = pr.repo

prUrl :: Config -> PR -> Text
prUrl config pr = [i|#{repoUrl config pr}/pulls/#{num}|]
 where
  num = pr.num

issueUrl :: Config -> PR -> Text
issueUrl config pr = [i|#{repoUrl config pr}/issues/#{num}|]
 where
  num = pr.num
