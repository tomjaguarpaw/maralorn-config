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

main :: IO ()
main = runEff entryPoint

webAppIO, termAppIO, guiAppIO :: IO ()
webAppIO = runEff webApp
termAppIO = runEff termApp
guiAppIO = runEff guiApp

webApp, termApp, guiApp :: e :> es => IOE e -> Eff es ()
webApp = \io -> runDomDialog io (runReflexDomServer 5344) app
termApp = \io -> runReflexHeadless (\r -> do runTermDialog io r (app io); pure never)
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

data NavState = StartPage | Doc Id

data Update = Next NavState | Save Doc NavState

nextState :: Update -> NavState
nextState = \case
  Next x -> x
  Save _ x -> x

effects :: Update -> Maybe Doc
effects = \case
  Save d _ -> Just d
  Next _ -> Nothing

app :: (e1 :> es, e2 :> es, Reflex.Reflex t) => IOE e1 -> Reflex Dialog t e2 -> Eff es ()
app = \io r -> mdo
  entries <- watchDB io r
  void $ performEffEvent r $ mapMaybe effects new_state_ev <&> writeDoc io
  nav_state <- reflex r $ holdDyn StartPage (nextState <$> new_state_ev)
  new_state_ev <-
    dynEffEv r
      $ nav_state
      <&> \case
        StartPage -> ReflexAction \r -> do
          text r "This is Kass. Your assistance to keep, arrange, schedule and succeed."
          newline r
          new_task_ev <- input r "Keep" "" <&> fmap (\t -> Save (newDoc & #content .~ t & #status ?~ Todo) StartPage)
          newline r
          dynEffEv r
            $ entries
            <&> \docs -> ReflexAction \r -> do
              doc_evs <- forM docs \e -> do
                newline r
                fmap (const (Next (Doc e.id)))
                  <$> button
                    r
                    (fromMaybe e.id.unId (headOf (#content % lined % folded) e))
              newline r
              pure $ leftmost $ new_task_ev : M.elems doc_evs
        Doc id' -> ReflexAction \r -> do
          ev1 <-
            dynEffEv r
              $ (M.lookup id' <$> entries)
              <&> \case
                Just doc -> ReflexAction \r -> do
                  ev' <- case doc.status of
                    Nothing -> pure never
                    Just Todo -> button r "☐" <&> fmap (const (Save (doc & #status % _Just .~ Done) (Doc id')))
                    Just Done -> button r "☑" <&> fmap (const (Save (doc & #status % _Just .~ Todo) (Doc id')))
                    Just x -> do text r (show x); pure never
                  new_content_ev <-
                    input r doc.content doc.content <&> fmap (\new_content -> (Save (doc & #content .~ new_content) (Doc id')))
                  newline r
                  ev2 <- button r "Delete" <&> fmap (const (Save (doc & #deleted .~ True) (Doc id')))
                  pure $ leftmost [ev', ev2, new_content_ev]
                Nothing -> ReflexAction \r -> do
                  text r [i|#{id'} missing|]
                  pure never
          ev2 <- footer r
          pure $ leftmost [ev1, ev2]
  pass
 where
  footer :: (e :> es, Reflex.Reflex t) => Reflex Dialog t e -> Eff es (Event t Update)
  footer = \r -> do
    newline r
    fmap (const (Next StartPage)) <$> button r "Back to start"
