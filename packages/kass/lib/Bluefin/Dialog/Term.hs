module Bluefin.Dialog.Term (runTermDialog) where

import Bluefin.Dialog
import Bluefin.Reflex
import Control.Concurrent.Async qualified as Async
import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Maralude
import Reflex hiding (Reflex)
import Reflex qualified
import System.Console.ANSI
  ( Color (..)
  , ColorIntensity (Vivid)
  , ConsoleLayer (..)
  , SGR (SetColor, SetDefaultColor)
  , clearScreen
  , setSGRCode
  )

data Update a = Return a | Prompt Text Text (Text -> a)

runTermDialog
  :: forall e1 e2 t es a s
   . (e1 :> es, e2 :> es, Reflex.Reflex t)
  => IOE e1
  -> Reflex s t e2
  -> (forall e. Reflex Dialog t e -> Eff (e :& es) a)
  -> Eff es a
runTermDialog = \io r act ->
  inContext'
    . inContext'
    . assoc1Eff
    . act @(e1 :& e2)
    $ ReflexHandle
      { payload =
          DialogHandle
            { run = \ev -> do
                (retEv, hook) <- reflex r newTriggerEvent
                _ <- runState Nothing $ \thread ->
                  performEffEvent r
                    $ ev
                    <&> \page -> do
                      whenJustM (get thread) (effIO io . Async.cancel)
                      put thread . Just =<< async io do
                        effIO io . hook =<< runPage io page
                pure retEv
            }
      , spiderData = mapHandle r.spiderData
      }

runPage :: e :> es => IOE e -> Page a -> Eff es a
runPage = \io page -> withEarlyReturn \ret -> forever do
  effIO io $ do clearScreen; putStr resetColor
  keybinds <- renderPage io page
  effIO io do putStr [i|#{color Magenta}> |]; hFlush stdout
  input <- effIO io getLine <&> preview (ix 0 % to (`Map.lookup` keybinds) % _Just)
  whenJust input
    $ returnEarly ret
    <=< \case
      Return val -> pure val
      Prompt prompt _ f -> do
        effIO io do
          putStr [i|#{color Magenta}#{prompt}> |]
          hFlush stdout
          f <$> getLine

color :: Color -> String
color c = setSGRCode [SetColor Foreground Vivid c]

resetColor :: String
resetColor = setSGRCode [SetDefaultColor Foreground]

renderPage :: e :> es => IOE e -> Page a -> Eff es (Map Char (Update a))
renderPage = \io page -> do
  execState mempty $ \st -> do
    let mkBind label update = do
          keybinds <- get st
          chooseHotkey (Map.keysSet keybinds) label & maybe
            (pure label)
            \key -> do
              put st $ Map.insert key update keybinds
              pure [i|#{color Magenta}#{Char.toUpper key}: #{color Blue}#{label}#{resetColor}|]

    forM_ page.lines \row -> do
      elems <- forM row.elems \case
        TextElement t -> pure t
        ButtonElement label value -> mkBind label (Return value)
        PromptElement prompt df f -> mkBind prompt (Prompt prompt df f)
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
