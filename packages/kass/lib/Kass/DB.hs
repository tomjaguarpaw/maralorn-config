module Kass.DB where

import Bluefin.Reflex
import Data.Aeson (FromJSON (..), eitherDecode, toJSON)
import Data.Map.Optics (toMapOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Kass.Doc
import Maralude
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens (responseBody)
import Reflex

newtype Row = MkRow {doc :: Doc}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype DocsResp = MkDocsResp {rows :: List Row}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ChangesResp = MkChangesResp
  { last_seq :: Text
  , results :: List Row
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

baseUrl :: String
baseUrl = "http://admin:admin@localhost:5984/kass"

getCouchJSON :: (FromJSON a, e :> es) => IOE e -> Text -> Eff es a
getCouchJSON = \io request -> do
  let req = [i|#{baseUrl}/#{request}|]
  resp <- effIO io $ Wreq.get req ^. mapping (lensVL responseBody)
  eitherDecode resp
    & either
      (\err -> effIO io . fail $ [i|Request: #{req}, Error: #{err}|])
      pure

rowsToMap :: [Row] -> Docs
rowsToMap = toMapOf (folded % #doc % ito \e -> (e.id, e))

getDB :: e :> es => IOE e -> Eff es Docs
getDB = \io -> getCouchJSON @DocsResp io "_all_docs?include_docs=true" ^. mapping (#rows % to rowsToMap)

writeDoc :: e :> es => IOE e -> Doc -> Eff es ()
writeDoc = \io -> \case
  d
    | Text.null d.id.unId -> void $ effIO io $ Wreq.post baseUrl (toJSON d)
    | x <- d.id.unId -> void $ effIO io $ Wreq.put [i|#{baseUrl}/#{x}|] (toJSON d)

watchDB :: (e1 :> es, e2 :> es, Reflex t) => IOE e1 -> ReflexE t e2 -> Eff es (Dynamic t Docs)
watchDB = \io r -> do
  initial_seq <- (.last_seq) <$> getCouchJSON @ChangesResp io "_changes?since=now"
  initialDocs <- getDB io
  (docUpdates, hook) <- reflex r newTriggerEvent
  _ <- async io $ runState initial_seq $ \st -> forever do
    last_seq <- get st
    resp <-
      getCouchJSON @ChangesResp
        io
        [i|_changes?since=#{last_seq}&feed=longpoll&include_docs=true&heartbeat=true|]
    effIO io $ hook (rowsToMap resp.results)
    put st resp.last_seq
  reflex r
    $ foldDyn
      (\update existing -> Map.filter (not . (.deleted)) $ Map.union update existing)
      initialDocs
      docUpdates
