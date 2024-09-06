module Bluefin.Dialog.Term (runTermDialog) where

import Bluefin.Compound
import Bluefin.Dialog
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Internal qualified as Internal
import Bluefin.Reflex
import Bluefin.State
import Bluefin.Stream
import Bluefin.Utils
import Control.Concurrent.Async qualified as Async
import Data.Char qualified as Char
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Optics
import Reflex hiding (Reflex)
import Reflex qualified
import Relude hiding (Handle, State, execState, get, put, runState)
import Relude.Unsafe qualified as Unsafe
import Say (say)
import System.Console.ANSI
  ( Color (..)
  , ColorIntensity (Vivid)
  , ConsoleLayer (..)
  , SGR (SetColor, SetDefaultColor)
  , clearScreen
  , setSGRCode
  )
import Witch (into)

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
        Internal.assoc1Eff $
          act $
            toDialogHandle (mapHandle stream) (mapHandle r)
  (elms, a) <-
    inContext'
      . inContext'
      . Internal.assoc1Eff
      $ yieldToList inner
  pageUpdate <- dynToEv r (distributeListOverDynWith fold elms)

  _ <- runState Nothing $ \thread ->
    performEffEvent r $
      pageUpdate
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
          DialogHandle $
            Internal.unsafeRemoveEff @e'
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
          Internal.unsafeRemoveEff @e' $ yield collector collected
          pure (result, result_ev)
      }
  mapAction
    :: (Reflex.Reflex t, es :> eb, Handle (h t))
    => ReflexAction Dialog t es b
    -> ReflexAction h t eb (b, Dynamic t (Seq (ElementData t)))
  mapAction = \(ReflexAction act) -> ReflexAction \h -> do
    (collected, result) <- yieldToList \collector ->
      Internal.inContext $ act $ go collector (mapHandle h)
    pure (result, distributeListOverDynWith fold collected)

data ElementData t where
  SimpleElement :: Element t () -> ElementData t
  ResponseElement :: Element t (Event t a) -> (a -> IO ()) -> ElementData t

runPage :: e :> es => IOE e -> Seq (ElementData t) -> Eff es ()
runPage = \io page -> forever do
  effIO io do clearScreen; putStr resetColor
  keybinds <- renderPage io page
  effIO io do putStr [i|#{color Magenta}> |]; hFlush stdout
  input' <- effIO io getLine <&> preview (to (`Map.lookup` keybinds) % _Just)
  whenJust input' $
    \case
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

renderPage :: e :> es => IOE e -> Seq (ElementData t) -> Eff es (Map Text Hook)
renderPage = \io page -> do
  execState mempty $ \(st :: State (Map Text Hook) st) -> do
    let
      mkBind :: st :> es => Text -> Hook -> Eff es Text
      mkBind label update = do
        keybinds <- get st
        chooseHotkey (Map.keysSet keybinds) label
          & \key -> do
            put st $ Map.insert key update keybinds
            pure [i|#{color Blue}#{label}#{color Magenta}#{Text.toUpper key}#{resetColor}|]
    elms <- forM page \case
      SimpleElement (TextElement t) -> pure (t <> " ")
      SimpleElement BreakElement -> pure "\n"
      ResponseElement (ButtonElement label) hook -> (<> " ") <$> mkBind label (SimpleHook (hook ()))
      ResponseElement (PromptElement prompt df) hook -> (<> " ") <$> mkBind prompt (PromptHook prompt df hook)
    effIO io $ say $ Text.intercalate "" $ into elms

execState :: s -> (forall (st :: Effects). State s st -> Eff (st :& es) a) -> Eff es s
execState s act = snd <$> runState s act

chooseHotkey :: Set Text -> Text -> Text
chooseHotkey used label = Unsafe.head . filter (`Set.notMember` used) $ candidate_keys
 where
  label_chars = into label
  ichars = zip [0 :: Int ..] candidate_chars
  candidate_keys = weight_sorted_product =<< [1 ..]
  weight_sorted_product n =
    fmap (toText . snd) $
      sortOn fst $
        fmap (\xs -> (sum (fst <$> xs), snd <$> xs)) $
          sort $
            replicateM n ichars

  candidate_chars =
    nubOrd $
      Char.toLower
        <$> filter Char.isUpper label_chars
          <> filter Char.isLower label_chars
          <> filter Char.isDigit label_chars
          <> "enaritudoschlgvfwkxqpmzbjy"
          <> ['0' .. '9']
