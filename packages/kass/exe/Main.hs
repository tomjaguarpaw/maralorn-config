module Main where

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
  let inTerm = runReflex (\r -> do runTermDialog io r (app io r); pure never)
  effIO io getArgs >>= \case
    [] -> inTerm
    ["term"] -> inTerm
    ["gui"] -> runReflexDomGUI (\r d -> (runDomDialog r d (app io r))) io
    ["web"] -> runReflexDomServer 7777 (\r d -> (runDomDialog r d (app io r))) io
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
        , [ButtonElement "70" 70, ButtonElement "42" 42]
        , [ButtonElement "Twentythree" 23, ButtonElement "Seventeeeeeen" (17 :: Int)]
        ]
  _ <-
    performEffEvent r
      $ ev
      <&> ( \case
              17 -> effIO io (say "Good choice")
              _ -> effIO io (say "Meh")
          )
  pure ()
