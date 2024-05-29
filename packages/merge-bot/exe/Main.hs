module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Aeson.Optics
import Data.String.Interpolate
import Data.Text qualified as Text
import Network.Wreq
import Optics
import Optics.Operators.Unsafe
import Relude
import System.Environment qualified as System

loadCredential :: String -> IO Text
loadCredential name = do
  credentials_directory <- System.getEnv "CREDENTIALS_DIRECTORY"
  Text.strip . decodeUtf8 <$> readFileBS (credentials_directory <> "/" <> name)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  [config_path] <- getArgs
  config <- readFileBS config_path
  token <- loadCredential "merge_bot_token"
  let url = config ^?! key "server" % _String
      repos = config ^.. key "repos" % values % _String
      botname = config ^?! key "botname" % filteredBy _String
      opts = defaults & lensVL auth ?~ oauth2Bearer (encodeUtf8 token)
  forever do
    for_ repos \repo -> do
      let repo_api = [i|#{url}/api/v1/repos/#{repo}|] :: Text
      putTextLn [i|Fetching open PRs from #{url}/#{repo}|]
      assigned_prs_resp <- getWith opts [i|#{repo_api}/pulls?state=open|]
      let prs = assigned_prs_resp ^.. lensVL responseBody % values % filteredBy (key "assignee" % key "login" % only botname)
      forM_ prs \pr -> do
        let num = pr ^?! key "number" % _Integer
            pr_base = pr ^?! key "base" % key "ref" % _String
            pr_head = pr ^?! key "head" % key "ref" % _String
            pr_head_commit = pr ^?! key "head" % key "sha" % _String
        commit_state_resp <- getWith opts [i|#{repo_api}/commits/#{pr_head_commit}/status|]
        putText [i|\##{num}: From #{pr_head} into #{pr_base}: |]
        let commit_state_body = commit_state_resp ^. lensVL responseBody
        case commit_state_body ^?! key "state" % _String of
          "success" -> do
            putText "CI success, "
            mergePR opts repo_api num pr_head_commit
          "" -> putTextLn "CI pending."
          "pending" -> putTextLn "CI pending."
          x -> do
            let timestamp =
                  Text.replace "+" "%2B" $
                    fromMaybe "updated at timestamp" $
                      maximumOf (key "statuses" % values % key "updated_at" % _String) commit_state_body
            putTextLn [i|CI #{x}.|]
            comments_resp <- getWith opts [i|#{repo_api}/issues/#{num}/comments?since=#{timestamp}|]
            when (hasn't (lensVL responseBody % values) comments_resp) do
              putTextLn "Adding comment"
              void $
                postWith
                  opts
                  [i|#{repo_api}/issues/#{num}/comments|]
                  (object ["body" .= ("CI failed" :: Text)])
    threadDelay 60_000_000

mergePR opts repo_api num head_commit = do
  _ <-
    postWith
      opts
      [i|#{repo_api}/pulls/#{num}/merge|]
      ( object
          [ "Do" .= ("fast-forward-only" :: Text)
          , "delete_branch_after_merge" .= True
          , "head_commit_id" .= head_commit
          ]
      )
  putTextLn "Merged."
