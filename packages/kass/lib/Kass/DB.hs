module Kass.DB where

import Bluefin.Reflex
import Data.Aeson (FromJSON (..), decode)
import Data.Map.Optics (toMapOf)
import Data.Map.Strict qualified as Map
import Kass.Doc
import Maralude
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens (responseBody)
import Optics.Operators.Unsafe qualified as Unsafe
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

getCouchJSON :: (FromJSON a, e :> es) => IOE e -> Text -> Eff es a
getCouchJSON = \io request -> do
  effIO io (Wreq.get [i|http://admin:admin@localhost:5984/kass/#{request}|])
    <&> (Unsafe.^?! (lensVL responseBody % to decode % _Just))

respToMap :: [Row] -> Docs
respToMap = toMapOf (folded % #doc % ito \e -> (e.id, e))

getDB :: e :> es => IOE e -> Eff es Docs
getDB = \io -> getCouchJSON @DocsResp io "_all_docs?include_docs=true" ^. mapping (#rows % to respToMap)

watchDB :: (e1 :> es, e2 :> es, Reflex t) => IOE e1 -> ReflexE t e2 -> Eff es (Dynamic t Docs)
watchDB = \io r -> do
  res <- (.last_seq) <$> getCouchJSON @ChangesResp io "_changes?since=now"
  initialDocs <- getDB io
  (docUpdates, hook) <- reflex r newTriggerEvent
  let getChanges = \last_seq -> do
        resp' <- getCouchJSON @ChangesResp io [i|_changes?since=#{last_seq}&feed=longpoll&include_docs=true&heartbeat=true|]
        effIO io $ hook (respToMap resp'.results)
        getChanges (resp'.last_seq)
  _ <- async io $ getChanges res
  reflex r $ foldDyn Map.union initialDocs docUpdates
