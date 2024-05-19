module Bluefin.Dialog.Term (runTermDialog) where

import Bluefin.Dialog
import Bluefin.Internal (unsafeRemoveEff)
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

runTermDialog
  :: forall e1 e2 t es a s
   . (e1 :> es, e2 :> es, Reflex.Reflex t, Handle (s t))
  => IOE e1
  -> Reflex s t e2
  -> (forall e. Reflex Dialog t e -> Eff (e :& es) a)
  -> Eff es a
runTermDialog = \io r act -> do
  let inner
        :: Stream (Dynamic t (Seq (ElementData t))) e3
        -> Eff (e3 :& ((e1 :& e2) :& es)) a
      inner = \stream ->
        assoc1Eff
          $ act
          $ toDialogHandle (mapHandle stream) (mapHandle r)
  (elms, a) <-
    inContext'
      . inContext'
      . assoc1Eff
      $ yieldToList inner
  pageUpdate <- dynToEv r (distributeListOverDynWith fold elms)

  _ <- runState Nothing $ \thread ->
    performEffEvent r
      $ pageUpdate
      <&> \page -> do
        whenJustM (get thread) (effIO io . Async.cancel)
        put thread . Just =<< async io do runPage io page
  pure a

toDialogHandle
  :: (Reflex.Reflex t, Handle (h t)) => Stream (Dynamic t (Seq (ElementData t))) e -> Reflex h t e -> Reflex Dialog t e
toDialogHandle = go
 where
  go
    :: forall h t e e'
     . (Reflex.Reflex t, Handle (h t))
    => Stream (Dynamic t (Seq (ElementData t))) e'
    -> Reflex h t e
    -> Reflex Dialog t e
  go = \collector r@ReflexHandle{runWithReplaceImpl} ->
    ReflexHandle
      { payload =
          DialogHandle
            $ unsafeRemoveEff @e'
            . \case
              (x@TextElement{}) -> yield collector . constDyn . one $ SimpleElement x
              (x@ButtonElement{}) -> do
                (ev, hook) <- reflex r newTriggerEvent
                yield collector . constDyn . one $ ResponseElement x hook
                pure ev
              (x@PromptElement{}) -> do
                (ev, hook) <- reflex r newTriggerEvent
                yield collector . constDyn . one $ ResponseElement x hook
                pure ev
              (x@BreakElement{}) -> yield collector . constDyn . one $ SimpleElement x
      , spiderData = mapHandle r.spiderData
      , runWithReplaceImpl = \initial ev -> do
          ((result, initial_dyn), later) <- runWithReplaceImpl (mapAction initial) (mapAction <$> ev)
          let (result_ev, dynEv) = splitE later
          collected <- reflex r $ join <$> holdDyn initial_dyn dynEv
          unsafeRemoveEff @e' $ yield collector collected
          pure (result, result_ev)
      }
  mapAction
    :: (Reflex.Reflex t, es :> eb, Handle (h t))
    => ReflexAction Dialog t es b
    -> ReflexAction h t eb (b, Dynamic t (Seq (ElementData t)))
  mapAction = \(ReflexAction act) -> ReflexAction \h -> do
    (collected, result) <- yieldToList \collector ->
      inContext $ act $ go collector (mapHandle h)
    pure (result, distributeListOverDynWith fold collected)

data ElementData t where
  SimpleElement :: Element t () -> ElementData t
  ResponseElement :: Element t (Event t a) -> (a -> IO ()) -> ElementData t

runPage :: e :> es => IOE e -> Seq (ElementData t) -> Eff es ()
runPage = \io page -> forever do
  effIO io do clearScreen; putStr resetColor
  keybinds <- renderPage io page
  effIO io do putStr [i|#{color Magenta}> |]; hFlush stdout
  input' <- effIO io getLine <&> preview (ix 0 % to (`Map.lookup` keybinds) % _Just)
  whenJust input'
    $ \case
      SimpleHook h -> effIO io h
      PromptHook prompt _ h -> do
        effIO io do
          putStr [i|#{color Magenta}#{prompt}> |]
          hFlush stdout
          h =<< getLine

color :: Color -> String
color c = setSGRCode [SetColor Foreground Vivid c]

resetColor :: String
resetColor = setSGRCode [SetDefaultColor Foreground]

data Hook = SimpleHook (IO ()) | PromptHook Text Text (Text -> IO ())

renderPage :: e :> es => IOE e -> Seq (ElementData t) -> Eff es (Map Char Hook)
renderPage = \io page -> do
  execState mempty $ \(st :: State (Map Char Hook) st) -> do
    let
      mkBind :: st :> es => Text -> Hook -> Eff es Text
      mkBind label update = do
        keybinds <- get st
        chooseHotkey (Map.keysSet keybinds) label & maybe
          (pure label)
          \key -> do
            put st $ Map.insert key update keybinds
            pure [i|#{color Magenta}#{Char.toUpper key}: #{color Blue}#{label}#{resetColor}|]
    elms <- forM page \case
      SimpleElement (TextElement t) -> pure (t <> " ")
      SimpleElement BreakElement -> pure "\n"
      ResponseElement (ButtonElement label) hook -> (<> " ") <$> mkBind label (SimpleHook (hook ()))
      ResponseElement (PromptElement prompt df) hook -> (<> " ") <$> mkBind prompt (PromptHook prompt df hook)
    effIO io $ say $ Text.intercalate "" $ into elms

execState :: s -> (forall st. State s st -> Eff (st :& es) a) -> Eff es s
execState s act = snd <$> runState s act

chooseHotkey :: Set Char -> Text -> Maybe Char
chooseHotkey used =
  view
    $ isomorph
    % to \label ->
      ( filter Char.isUpper label
          <> filter Char.isLower label
          <> filter Char.isDigit label
          <> "enaritudoschlgvfwkxqpmzbä,ö.üj"
          <> ['a' .. 'z']
          <> ['0' .. '9']
      )
        ^? pre (folded % to Char.toLower % filtered (`Set.notMember` used))
