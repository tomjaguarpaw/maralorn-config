{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Kass.App where

import Bluefin.Dialog
import Bluefin.Dialog.ReflexDom
import Bluefin.Dialog.Term
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Bluefin.Reflex.Headless
import Data.Map.Strict qualified as M
import Kass.DB
import Kass.Doc
import Maralude
import Reflex hiding (Reflex)
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

nextState :: Seq Update -> Maybe NavState
nextState = lastOf (folded % #_Next)

app :: (e1 :> es, e2 :> es) => IOE e1 -> Reflex Dialog t e2 -> Eff es ()
app = \io r -> withReflex r mdo
  entries <- watchDB io r
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

schedule :: Dynamic t Docs -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
schedule = \_entries r -> do
  ev_header <- header Schedule r
  newline r
  text r "TBD"
  pure ev_header

showDoc :: Id -> Dynamic t Docs -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
showDoc = \id' entries r -> withReflex r do
  ev_header <- header (Doc id') r
  ev_doc <-
    dynEffEv r $
      (M.lookup id' <$> entries)
        <&> \case
          Just doc -> ReflexAction \r -> do
            ev' <- case doc.status of
              Nothing -> pure never
              Just Todo -> button r "☐" <&> fmap (const (one $ Save (doc & #status % _Just .~ Done)))
              Just Done -> button r "☑" <&> fmap (const (one $ Save (doc & #status % _Just .~ Todo)))
              Just x -> do text r (show x); pure never
            new_content_ev <-
              input r doc.content doc.content
                <&> fmap (\new_content -> (one $ Save (doc & #content .~ new_content)))
            newline r
            ev2 <- button r "Delete" <&> fmap (const (Save (doc & #deleted .~ True) <| one (Next Schedule)))
            pure $ leftmost [ev', ev2, new_content_ev]
          Nothing -> ReflexAction \r -> do
            text r [i|#{id'} missing|]
            pure never
  pure $ ev_doc <> ev_header

sort' :: Dynamic t Docs -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
sort' = \_entries r -> do
  ev_header <- header Sort r
  newline r
  text r "TBD"
  pure ev_header

search :: Dynamic t Docs -> Reflex Dialog t ei -> Eff (ei :& es) (Event t (Seq Update))
search = \entries r -> withReflex r do
  ev_header <- header Search r
  ev_list <-
    dynEffEv r $
      entries
        <&> \docs -> ReflexAction \r -> do
          doc_evs <- forM docs \e -> do
            newline r
            docItem r e
          newline r
          pure $ fold (M.elems doc_evs)
  pure $ ev_header <> ev_list

docItem :: e :> es => Reflex Dialog t e -> Doc -> Eff es (Event t (Seq Update))
docItem = \r doc -> withReflex r do
  ev' <- case doc.status of
    Nothing -> pure never
    Just Todo -> button r "☐" <&> fmap (const (one $ Save (doc & #status % _Just .~ Done)))
    Just Done -> button r "☑" <&> fmap (const (one $ Save (doc & #status % _Just .~ Todo)))
    Just x -> do text r (show x); pure never
  open_ev <- button r doc.content <&> fmap (const (one $ Next (Doc doc.id)))
  pure $ leftmost [ev', open_ev]
