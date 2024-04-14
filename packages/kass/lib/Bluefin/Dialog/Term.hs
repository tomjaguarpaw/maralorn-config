module Bluefin.Dialog.Term where

import Bluefin.Dialog
import Bluefin.Reflex
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Data.Char qualified as Char
import Data.Map.Lazy qualified as LMap
import Data.Set qualified as Set
import Data.Text qualified as Text
import Maralude
import Reflex
import System.Console.ANSI
  ( Color (..)
  , ColorIntensity (Vivid)
  , ConsoleLayer (..)
  , SGR (SetColor, SetDefaultColor)
  , clearScreen
  , setSGRCode
  )

runTermDialog
  :: forall e1 e2 t es a
   . (e1 :> es, e2 :> es, Reflex t)
  => IOE e1
  -> ReflexE t e2
  -> (forall e. Dialog t e -> Eff (e :& es) a)
  -> Eff es a
runTermDialog = \io r act ->
  inContext'
    . inContext'
    . assoc1Eff
    . act @(e1 :& e2)
    $ MkDialog
      { run = \ev -> do
          (retEv, hook) <- reflex r newTriggerEvent
          _ <- runState Nothing $ \thread ->
            performEffEvent r
              $ ev
              <&> \page -> do
                whenJustM (get thread) (effIO io . Async.cancel)
                put thread . Just =<< async io do
                  effIO io . hook =<< withEarlyReturn \ret -> forever do
                    effIO io $ do clearScreen; putStr resetColor
                    keybinds <- renderPage io page
                    effIO io do putStr [i|#{color Magenta}> |]; hFlush stdout
                    input <- effIO io getLine <&> preview (ix 0 % to (`LMap.lookup` keybinds) % _Just)
                    whenJust input (returnEarly ret)
          pure retEv
      , r = mapHandle r
      }

color :: Color -> String
color c = setSGRCode [SetColor Foreground Vivid c]

resetColor :: String
resetColor = setSGRCode [SetDefaultColor Foreground]

async :: e :> es => IOE e -> Eff es a -> Eff es (Async a)
async = \io act -> withEffToIO (\runInIO -> Async.async $ runInIO (const (useImpl act))) io

renderPage :: e :> es => IOE e -> Page a -> Eff es (Map Char a)
renderPage = \io page -> do
  execState mempty $ \st -> do
    forM_ page.lines \row -> do
      elems <- forM row.elems \case
        TextElement t -> pure t
        ButtonElement label value -> do
          keybinds <- get st
          chooseHotkey (LMap.keysSet keybinds) label & maybe
            (pure label)
            \key -> do
              put st $ LMap.insert key value keybinds
              pure
                [i|#{color Magenta}#{Char.toUpper key}: #{color Blue}#{label}#{resetColor}|]
        _ -> error "not supported"
      effIO io . say . Text.intercalate " " $ elems

execState :: s -> (forall st. State s st -> Eff (st :& es) a) -> Eff es s
execState s act = snd <$> runState s act

chooseHotkey :: Set Char -> Text -> Maybe Char
chooseHotkey used =
  view
    $ isomorph
    % to \label ->
      ( filter Char.isUpper label
          <> filter Char.isLower label
          <> label
          <> "enaritudoschlgvfwkxqpmzbä,ö.üj"
          <> ['a' .. 'z']
          <> ['0' .. '9']
      )
        ^? pre (folded % to Char.toLower % filtered (`Set.notMember` used))
