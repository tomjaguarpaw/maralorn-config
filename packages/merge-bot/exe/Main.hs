module Main where

import Data.Aeson
import Data.Aeson.Optics
import Data.String.Interpolate
import Network.Wreq
import Optics
import Optics.Operators.Unsafe
import Relude

url, owner, repo, repoApi :: Text
url = "https://code.maralorn.de"
owner = "maralorn"
repo = "config"

botname :: Value
botname = "marabot"

repoApi = [i|#{url}/api/v1/repos/#{owner}/#{repo}|]

main :: IO ()
main = do
  [token] <- fmap encodeUtf8 <$> getArgs
  let opts = defaults & lensVL auth ?~ oauth2Bearer token
  resp' <- getWith opts [i|#{repoApi}/pulls?state=open|]
  let prs = resp' ^.. lensVL responseBody % values % filteredBy (key "assignee" % key "login" % only botname)
  forM_ prs \pr -> do
    let num = pr ^?! key "number" % _Integer
        pr_base = pr ^?! key "base" % key "ref" % _String
        pr_head = pr ^?! key "head" % key "ref" % _String
    resp'' <-
      postWith
        (opts & lensVL checkResponse ?~ \_ _ -> pure ())
        [i|#{repoApi}/pulls/#{num}/merge|]
        ( object
            [ "Do" .= ("fast-forward-only" :: Text)
            , "delete_branch_after_merge" .= True
            , "merge_when_checks_succeed" .= True
            ]
        )
    putText [i|\##{num}: From #{pr_head} into #{pr_base}: |]
    case resp'' ^. lensVL responseStatus % lensVL statusCode of
      200 -> putTextLn "Activated automerge"
      409 -> do
        putText "No Op: "
        forOf_ (lensVL responseBody % key "message" % _String) resp'' putTextLn
      x -> putTextLn [i|Unexpected exit code #{x}: #{resp'' ^. lensVL responseBody}|]
    pure ()
