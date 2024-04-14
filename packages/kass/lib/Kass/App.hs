module Kass.App where

import Bluefin.Dialog
import Bluefin.Dialog.ReflexDom
import Bluefin.Dialog.Term
import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Bluefin.Reflex.Headless
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

data NavState = StartPage | Numbers | Congrats | Disappointment

type Update = NavState

processUpdate :: Update -> NavState
processUpdate = id

viewState :: NavState -> Page Update
viewState =
  \case
    StartPage -> line (txt "Hello World!") <> line (button "Start numbers game" Numbers)
    Numbers ->
      line (txt "What is the best number?")
        <> line (txt "An even one?" <> button "70" Disappointment <> button "42" Disappointment)
        <> line (txt "Or an ott one?" <> button "Twentythree" Disappointment <> button "Seventeeeeeen" Congrats)
        <> line (txtField "Enter Other Number" "" (\case "17" -> Congrats; _ -> Disappointment))
        <> footer
    Congrats -> line (txt "Good choice") <> footer
    Disappointment -> line (txt "Meh") <> footer
 where
  footer = line mempty <> line (button "Back to start" StartPage)

app :: (e1 :> es, e2 :> es, e3 :> es, Reflex t) => IOE e1 -> ReflexE t e2 -> Dialog t e3 -> Eff es ()
app = \_ r dialog -> mdo
  state <- reflex r $ holdDyn StartPage (processUpdate <$> newState)
  newState <- showPage dialog $ viewState <$> state
  pass
