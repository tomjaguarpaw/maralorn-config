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
              Just Todo -> button r (nf "md-checkbox_blank_outline" 0xf0131) <&> fmap (const (one $ Save (doc & #status % _Just .~ Done)))
              Just Done -> button r (nf "md-checkbox_outline" 0xf0c52) <&> fmap (const (one $ Save (doc & #status % _Just .~ Todo)))
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
          let list = Seq.sortOn (.priority) $ Seq.fromList $ filter (hasn't (#status % _Just % #_Done)) (toList docs)
          doc_evs <- forM list \e -> do
            newline r
            docItem r list e
          newline r
          pure $ fold doc_evs
  pure $ ev_header <> ev_list

nf :: Text -> Int -> Text
nf _ = Text.singleton . toEnum

docItem :: e :> es => Reflex Dialog t e -> Seq Doc -> Doc -> Eff es (Event t (Seq Update))
docItem r docs doc = withReflex r do
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
  (before, after') = Seq.breakl ((== doc.id) . (.id)) docs
  after = Seq.drop 1 after'
