module Kass.App where

import Bluefin.Dialog
import Bluefin.Dialog.ReflexDom
import Bluefin.Dialog.Term
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Bluefin.Reflex.Headless
import Kass.DB
import Kass.Doc
import Maralude
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

viewState :: Docs -> NavState -> Page Update
viewState = \docs ->
  \case
    StartPage ->
      line (txt "This is Kass. Your assistance to keep, arrange, schedule and succeed.")
        <> line (txtField "Keep" "" (\t -> Save (newDoc & #content .~ t & #status ?~ Todo) StartPage))
        <> foldOf
          ( folded
              % to
                ( \e ->
                    button
                      (fromMaybe e.id.unId (headOf (#content % lined % folded) e))
                      (Next (Doc e.id))
                )
              % to line
          )
          docs
    Doc id' -> line (txt id'.unId) <> footer
 where
  footer = line mempty <> line (button "Back to start" (Next StartPage))

app :: (e1 :> es, e2 :> es, e3 :> es, Reflex t) => IOE e1 -> ReflexE t e2 -> Dialog t e3 -> Eff es ()
app = \io r dialog -> mdo
  entries <- watchDB io r
  state <- reflex r $ holdDyn StartPage (nextState <$> newState)
  newState <- showPage dialog $ viewState <$> entries <*> state
  void $ performEffEvent r $ mapMaybe effects newState <&> writeDoc io
