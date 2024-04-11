module Main where

import Bluefin.Reflex
import Bluefin.Reflex.Dom
import Maralude
import Reflex
import Reflex.Dom

app
  :: (e :> es, ei :> es, Reflex t)
  => IOE ei
  -> ReflexE t e
  -> Eff es (Event t Int)
app io r = do
  let myDyn = constDyn (5 :: Int)
  pb <- reflex r getPostBuild
  pbs <- reflex r $ foldDyn (\_ x -> x + 1) 0 pb
  foo <-
    performEffEvent r
      $ updated ((+) <$> myDyn <*> pbs)
      <&> \x -> do
        effIO io $ print x
        pure x
  reflexIO io r (delay 5 foo)

main :: IO ()
main = runEff \io -> runReflexDom (widget io) io

widget :: (Reflex t, e1 :> es, e2 :> es) => IOE e1 -> ReflexE t e2 -> Dom t e2 -> Eff es ()
widget =
  \io r d -> do
    pb <- reflex r getPostBuild
    let ev1 = "Hello World!" <$ pb
    ev2 <- reflexIO io r (delay 5 ("Hello World again!" <$ ev1))
    msg <- reflex r $ holdDyn "Not Hello!" (leftmost [ev1, ev2])
    dom r d (dynText msg)
