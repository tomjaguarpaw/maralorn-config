{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Kass.App where

import Bluefin.Dialog
import Bluefin.Dialog.ReflexDom
import Bluefin.Dialog.Term
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Bluefin.Reflex.Headless
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Kass.DB
import Kass.Doc
import Kass.Sort
import Optics
import Reflex hiding (Reflex)
import Relude
import Witherable qualified

main :: IO ()
main = runEff entryPoint

webAppIO, termAppIO :: IO ()
webAppIO = runEff webApp
termAppIO = runEff termApp

webApp, termApp :: e :> es => IOE e -> Eff es ()
webApp = \io -> runDomDialog io (runReflexDomServer 5344) app
termApp = \io -> runReflexHeadless (\r -> do runTermDialog io r (app io); pure never)

entryPoint
  :: e :> es
  => IOE e
  -> Eff es ()
entryPoint = \io -> do
  effIO io getArgs >>= \case
    [] -> termApp io
    ["term"] -> termApp io
    ["web"] -> webApp io
    _ -> error "not implemented"

data NavState = Schedule | Doc Id | Sort | Search deriving stock (Eq)

data Update = Next NavState | Save Doc
  deriving stock (Generic)

type AppState = Map Id DocState

data DocState = MkDocState
  { doc :: Doc
  , children :: Map Id DocState
  , parent :: Maybe DocState
  }
  deriving stock (Generic)

mkState :: Docs -> AppState
mkState docs = app_state
 where
  children =
    foldl' (Map.unionWith (<>)) mempty $
      toList app_state <&> \ds -> case ds.doc.parent of
        Just parent_id -> Map.singleton parent_id (Map.singleton ds.doc.id ds)
        Nothing -> mempty
  app_state =
    docs <&> \doc ->
      MkDocState
        { doc
        , children = fromMaybe mempty $ M.lookup doc.id children
        , parent = (`M.lookup` app_state) =<< doc.parent
        }

nextState :: Seq Update -> Maybe NavState
nextState = lastOf (folded % #_Next)

app :: (e1 :> es, e2 :> es) => IOE e1 -> Reflex Dialog t e2 -> Eff es ()
app = \io r -> withReflex r mdo
  entries <- watchDB io r <&> fmap mkState
  void $
    performEffEvent r $
      fmap (mapM_ (writeDoc io)) . Witherable.filter (not . null) . fmap (toListOf (folded % #_Save)) $
        new_state_ev
  nav_state <- reflex r $ holdDyn Schedule (fmapMaybe nextState new_state_ev)
  new_state_ev <-
    dynEffEv r $
      nav_state
        <&> \case
          Schedule -> ReflexAction (schedule entries)
          Doc id' -> ReflexAction (showDoc id' entries)
          Sort -> ReflexAction (sort' entries)
          Search -> ReflexAction (search entries)
  pass

header :: e :> es => NavState -> Reflex Dialog t e -> Eff es (Event t (Seq Update))
header = \nav_state r -> withReflex r do
  text r "KASS:"
  ev_new_task <- input r "Collect" "" <&> fmap (\t -> one $ Save (newDoc & #content .~ t & #status ?~ Todo))
  ev_arrange <-
    if nav_state /= Sort
      then button r "Sort"
      else text r "Sort" >> pure never
  ev_schedule <-
    if nav_state /= Search
      then button r "Plan"
      else text r "Plan" >> pure never
  ev_succeed <-
    if nav_state /= Schedule
      then button r "Execute"
      else text r "Execute" >> pure never
  newline r
  pure $
    ev_new_task
      <> (one (Next Sort) <$ ev_arrange)
      <> (one (Next Search) <$ ev_schedule)
      <> (one (Next Schedule) <$ ev_succeed)

schedule :: Dynamic t AppState -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
schedule = \_entries r -> do
  ev_header <- header Schedule r
  newline r
  text r "TBD"
  pure ev_header

showDoc :: Id -> Dynamic t AppState -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
showDoc = \id' entries r -> withReflex r do
  ev_header <- header (Doc id') r
  ev_doc <-
    dynEffEv r $
      (M.lookup id' <$> entries)
        <&> \case
          Just ds -> ReflexAction \r -> do
            ev' <- case ds.doc.status of
              Nothing -> pure never
              Just Todo -> button r (nf "md-checkbox_blank_outline" 0xf0131) <&> fmap (const (one $ Save (ds.doc & #status % _Just .~ Done)))
              Just Done -> button r (nf "md-checkbox_outline" 0xf0c52) <&> fmap (const (one $ Save (ds.doc & #status % _Just .~ Todo)))
              Just x -> do text r (show x); pure never
            new_content_ev <-
              input r ds.doc.content ds.doc.content
                <&> fmap (\new_content -> (one $ Save (ds.doc & #content .~ new_content)))
            newline r
            ev2 <- button r "Delete" <&> fmap (const (Save (ds.doc & #deleted .~ True) <| one (Next Schedule)))
            parent_ev <- case ds.parent of
              Just parent -> do
                text r "Parent:"
                docItem r mempty parent
              Nothing -> pure never

            rec pick <- reflex r $ holdDyn Nothing $ Just <$> current entries <@ ffilter isNothing parent_set_ev
                parent_set_ev <-
                  dynEffEv r $
                    pick <&> \case
                      Just entr -> ReflexAction \r -> do
                        text r "Pick parent:"
                        fmap (fmap Just . leftmost) $ forM (toList entr) $ \ds -> do
                          button r ds.doc.content <&> ($> ds.doc.id)
                      Nothing -> ReflexAction \r -> button r "Pick parent" <&> ($> Nothing)
            let new_parent_ev = Witherable.catMaybes parent_set_ev <&> \id -> one (Save $ ds.doc & #parent ?~ id)
            childs_ev <-
              if Map.null ds.children
                then pure never
                else do
                  text r "Children:"
                  showDocs r ds.children
            pure $ leftmost [ev', ev2, new_content_ev, childs_ev, parent_ev, new_parent_ev]
          Nothing -> ReflexAction \r -> do
            text r [i|#{id'} missing|]
            pure never
  pure $ ev_doc <> ev_header

sort' :: Dynamic t AppState -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
sort' = \_entries r -> do
  ev_header <- header Sort r
  newline r
  text r "TBD"
  pure ev_header

showDocs :: e :> es => Reflex Dialog t e -> Map Id DocState -> Eff es (Event t (Seq Update))
showDocs r docs = withReflex r do
  let list = Seq.sortOn (.doc.priority) $ Seq.fromList $ filter (hasn't (#doc % #status % _Just % #_Done)) (toList docs)
  doc_evs <- forM list \e -> do
    newline r
    docItem r list e
  newline r
  pure $ fold doc_evs

search :: Dynamic t AppState -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
search = \entries r -> withReflex r do
  ev_header <- header Search r
  ev_list <-
    dynEffEv r $
      entries
        <&> \docs -> ReflexAction (`showDocs` (Witherable.filter (hasn't (#parent % _Just)) docs))
  pure $ ev_header <> ev_list

nf :: Text -> Int -> Text
nf _ = Text.singleton . toEnum

docItem :: e :> es => Reflex Dialog t e -> Seq DocState -> DocState -> Eff es (Event t (Seq Update))
docItem r docs ds = withReflex r do
  ev' <- case doc.status of
    Nothing -> pure never
    Just Todo -> button r (nf "md-checkbox_blank_outline" 0xf0131) <&> fmap (const (one $ Save (doc & #status % _Just .~ Done)))
    Just Done -> button r (nf "md-checkbox_outline" 0xf0c52) <&> fmap (const (one $ Save (doc & #status % _Just .~ Todo)))
    Just x -> do text r (show x); pure never
  open_ev <- button r doc.content <&> fmap (const (one $ Next (Doc doc.id)))
  up_ev <- case before of
    rest :|> p -> do
      let new = Seq.fromList . fmap Save . toList $ setPriorities (rest >< (doc <| p <| after))
      button r (nf "md-arrow_up" 0xf005d) <&> fmap (const new)
    _ -> pure never
  down_ev <- case after of
    p :<| rest -> do
      let new = Seq.fromList . fmap Save . toList $ setPriorities (before >< (p <| doc <| rest))
      button r (nf "md-arrow_down" 0xf0045) <&> fmap (const new)
    _ -> pure never
  pure $ leftmost [ev', open_ev, up_ev, down_ev]
 where
  doc = ds.doc
  (before, after') = Seq.breakl ((== doc.id) . (.id)) $ (.doc) <$> docs
  after = Seq.drop 1 after'
