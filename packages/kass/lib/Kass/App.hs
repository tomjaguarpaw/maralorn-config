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
import Reflex qualified
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

data NavState = StartPage | Doc Id

data Update = Next NavState | Save Doc
  deriving stock (Generic)

nextState :: Seq Update -> Maybe NavState
nextState = lastOf (folded % #_Next)

app :: (e1 :> es, e2 :> es, Reflex.Reflex t) => IOE e1 -> Reflex Dialog t e2 -> Eff es ()
app = \io r -> mdo
  entries <- watchDB io r
  void
    $ performEffEvent r
    $ fmap (mapM_ (writeDoc io))
    . Witherable.filter (not . null)
    . fmap (toListOf (folded % #_Save))
    $ new_state_ev
  nav_state <- reflex r $ holdDyn StartPage (fmapMaybe nextState new_state_ev)
  new_state_ev <-
    dynEffEv r
      $ nav_state
      <&> \case
        StartPage -> ReflexAction \r -> do
          text r "This is Kass. Your assistance to keep, arrange, schedule and succeed."
          newline r
          new_task_ev <- input r "Keep" "" <&> fmap (\t -> one $ Save (newDoc & #content .~ t & #status ?~ Todo))
          newline r
          dynEffEv r
            $ entries
            <&> \docs -> ReflexAction \r -> do
              doc_evs <- forM docs \e -> do
                newline r
                docItem r e
              newline r
              pure $ fold (new_task_ev : M.elems doc_evs)
        Doc id' -> ReflexAction \r -> do
          ev1 <-
            dynEffEv r
              $ (M.lookup id' <$> entries)
              <&> \case
                Just doc -> ReflexAction \r -> do
                  ev' <- case doc.status of
                    Nothing -> pure never
                    Just Todo -> button r "☐" <&> fmap (const (one $ Save (doc & #status % _Just .~ Done)))
                    Just Done -> button r "☑" <&> fmap (const (one $ Save (doc & #status % _Just .~ Todo)))
                    Just x -> do text r (show x); pure never
                  new_content_ev <-
                    input r doc.content doc.content <&> fmap (\new_content -> (one $ Save (doc & #content .~ new_content)))
                  newline r
                  ev2 <- button r "Delete" <&> fmap (const (Save (doc & #deleted .~ True) <| one (Next StartPage)))
                  pure $ leftmost [ev', ev2, new_content_ev]
                Nothing -> ReflexAction \r -> do
                  text r [i|#{id'} missing|]
                  pure never
          ev2 <- footer r
          pure $ leftmost [ev1, ev2]
  pass
 where
  footer :: (e :> es, Reflex.Reflex t) => Reflex Dialog t e -> Eff es (Event t (Seq Update))
  footer = \r -> do
    newline r
    fmap (const (one $ Next StartPage)) <$> button r "Back to start"

docItem :: (Reflex.Reflex t, e :> es) => Reflex Dialog t e -> Doc -> Eff es (Event t (Seq Update))
docItem = \r doc -> do
  ev' <- case doc.status of
    Nothing -> pure never
    Just Todo -> button r "☐" <&> fmap (const (one $ Save (doc & #status % _Just .~ Done)))
    Just Done -> button r "☑" <&> fmap (const (one $ Save (doc & #status % _Just .~ Todo)))
    Just x -> do text r (show x); pure never
  open_ev <- button r doc.content <&> fmap (const (one $ Next (Doc doc.id)))
  pure $ leftmost [ev', open_ev]
