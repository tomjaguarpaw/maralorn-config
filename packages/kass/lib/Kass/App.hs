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

app :: (e1 :> es, e2 :> es, e3 :> es, Reflex t) => IOE e1 -> ReflexE t e2 -> Dialog t e3 -> Eff es ()
app = \io r dialog -> do
  pb <- reflex r getPostBuild
  ev <-
    showPage dialog
      $ pb
      $> MkPage
        [ [TextElement "Hello World!"]
        , [TextElement "What is the best number?"]
        , [TextElement "An even one?", ButtonElement "70" 70, ButtonElement "42" 42]
        , [TextElement "Or an ott one?", ButtonElement "Twentythree" 23, ButtonElement "Seventeeeeeen" (17 :: Int)]
        ]
  _ <-
    performEffEvent r
      $ ev
      <&> ( \case
              17 -> effIO io (say "Good choice")
              _ -> effIO io (say "Meh")
          )
  pure ()
