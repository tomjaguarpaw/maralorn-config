module Cass.App where

import Bluefin.Dialog
import Bluefin.Dialog.ReflexDom
import Bluefin.Dialog.Term
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Bluefin.Reflex.Headless
import Data.Aeson (FromJSON (..), Key, Object, ToJSON (..), Value (..), decode, withObject, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Map.Optics (toMapOf)
import Data.Map.Strict qualified as Map
import Maralude
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens (responseBody)
import Optics.Operators.Unsafe qualified as Unsafe
import Reflex

main :: IO ()
main = runEff entryPoint

webAppIO, termAppIO, guiAppIO :: IO ()
webAppIO = runEff webApp
termAppIO = runEff termApp
guiAppIO = runEff guiApp

webApp, termApp, guiApp :: e :> es => IOE e -> Eff es ()
webApp = \io -> runDomDialog io (runReflexDomServer 5344) app
termApp = \io -> runReflexHeadless (\r -> do runTermDialog io r (app io r); pure never)
guiApp = \io -> runDomDialog io runReflexDomGUI app

entryPoint
  :: e :> es
  => IOE e
  -> Eff es ()
entryPoint = \io -> do
  effIO io getArgs >>= \case
    [] -> termApp io
    ["term"] -> termApp io
    ["gui"] -> guiApp io
    ["web"] -> webApp io
    _ -> error "not implemented"

data NavState = StartPage | Entry Text

data Update = Next NavState | Save Entry NavState

processUpdate :: Update -> NavState
processUpdate = \case
  Next x -> x
  _ -> _

viewState :: Map Text Entry -> NavState -> Page Update
viewState = \entries ->
  \case
    StartPage ->
      line (txt "Hello World!")
        <> foldOf
          ( folded
              % to
                ( \e ->
                    button
                      (fromMaybe e.id (headOf (#content % lined % folded) e))
                      (Next (Entry e.id))
                )
              % to line
          )
          entries
    Entry id' -> line (txt id') <> footer
 where
  footer = line mempty <> line (button "Back to start" (Next StartPage))

app :: (e1 :> es, e2 :> es, e3 :> es, Reflex t) => IOE e1 -> ReflexE t e2 -> Dialog t e3 -> Eff es ()
app = \io r dialog -> mdo
  entries <- watchDB io r
  state <- reflex r $ holdDyn StartPage (processUpdate <$> newState)
  newState <- showPage dialog $ viewState <$> entries <*> state
  pass

newtype Row = MkRow {doc :: Entry}
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
  effIO io (Wreq.get [i|http://admin:admin@localhost:5984/cass/#{request}|])
    <&> (Unsafe.^?! (lensVL responseBody % to decode % _Just))

watchDB :: (e1 :> es, e2 :> es, Reflex t) => IOE e1 -> ReflexE t e2 -> Eff es (Dynamic t (Map Text Entry))
watchDB = \io r -> do
  res <- (.last_seq) <$> getCouchJSON @ChangesResp io "_changes?since=now"
  resp <- getCouchJSON @DocsResp io "_all_docs?include_docs=true"
  (ev, hook) <- reflex r newTriggerEvent
  let respToMap = toMapOf (folded % #doc % ito \e -> (e.id, e))
      getChanges = \last_seq -> do
        resp' <- getCouchJSON @ChangesResp io [i|_changes?since=#{last_seq}&feed=longpoll&include_docs=true&heartbeat=true|]
        effIO io $ hook (respToMap resp'.results)
        getChanges (resp'.last_seq)
  _ <- async io $ getChanges res
  reflex r $ foldDyn Map.union (respToMap resp.rows) ev

data Entry = MkEntry
  { id :: Text
  , rev :: Maybe Text
  , content :: Text
  , rest :: Map Text Value
  }
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Entry where
  parseJSON = withObject "Entry" \v -> do
    (id', v1) <- stripKey v "_id"
    (rev, v2) <- stripOptionalKey v1 "_rev"
    (content, v3) <- stripOptionalKey v2 "content"
    pure
      $ MkEntry
        { content = fromMaybe "" content
        , id = id'
        , rev
        , rest = KeyMap.toMapText v3
        }

stripOptionalKey :: FromJSON a => Object -> Key -> Parser (Maybe a, Object)
stripOptionalKey = \v k -> do
  val <- v .:? k
  pure (val, KeyMap.delete k v)

stripKey :: FromJSON a => Object -> Key -> Parser (a, Object)
stripKey = \v k -> do
  val <- v .: k
  pure (val, KeyMap.delete k v)

instance ToJSON Entry where
  toJSON = \e ->
    Object
      . KeyMap.insert "_id" (String e.id)
      . KeyMap.insert "content" (String e.content)
      . maybe id (\r -> KeyMap.insert "_rev" (String r)) e.rev
      . KeyMap.fromMapText
      $ e.rest
