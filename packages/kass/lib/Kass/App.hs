module Kass.App (main) where

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

entryPoint
  :: e :> es
  => IOE e
  -> Eff es ()
entryPoint = \io -> do
  let inTerm = runReflexHeadless (\r -> do runTermDialog io r (app io r); pure never)
      webApp = runDomDialog io (runReflexDomServer 5344) app
  effIO io getArgs >>= \case
    [] -> inTerm
    ["term"] -> inTerm
    ["gui"] -> runDomDialog io runReflexDomGUI app
    ["web"] -> webApp
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
