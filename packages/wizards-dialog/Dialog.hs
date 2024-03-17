{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Dialog
  ( runHaskeline
  , runClearingHaskeline
  , Menu (..)
  , MenuEntry (..)
  , menu
  , confirm
  , getLineWithDefaultAndSuggestions
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (mzero))
import Control.Monad.IO.Class
import Data.Char
  ( isLower
  , isUpper
  , toLower
  , toUpper
  )
import Data.List
import Data.Maybe
import Data.Text (Text)
import System.Console.ANSI
import System.Console.Haskeline
import System.Console.Wizard
import System.Console.Wizard.Haskeline
import System.Console.Wizard.Internal
import Witch

newtype ClearingHaskeline a = ClearingHaskeline {runClearing :: InputT IO a}
  deriving newtype (Functor, Applicative, Monad)

instance Run ClearingHaskeline Haskeline where
  runAlgebra =
    ClearingHaskeline . prependClearing . runAlgebra . fmap runClearing
   where
    prependClearing a = liftIO (clearScreen >> setCursorPosition 0 0) >> a

data MenuEntry a = Option Text a | SubMenu (Menu a) deriving (Show)

data Menu a = Menu Text [MenuEntry a]
  deriving (Show)

cancelCharacter :: Char
cancelCharacter = '.'

menu
  :: forall a m
   . (Character :<: m, Show a)
  => Maybe Text
  -> Menu a
  -> Wizard m a
menu = runMenu True
 where
  runMenu top prompt thisMenu@(Menu name options) = do
    response <-
      retry . validator (`elem` hotkeys) $
        character
          (intercalate "\n" wholePrompt)
    case lookup (toLower response) mappings of
      Just (Option _ a) -> pure a
      Just (SubMenu submenu) ->
        runMenu False prompt submenu <|> runMenu True prompt thisMenu
      Nothing -> mzero
   where
    hotkeys :: [Char]
    mappings :: [(Char, MenuEntry a)]
    (hotkeys, mappings) = foldr foldMappings ([cancelCharacter], []) options

    foldMappings
      :: MenuEntry a
      -> ([Char], [(Char, MenuEntry a)])
      -> ([Char], [(Char, MenuEntry a)])
    foldMappings option accu@(accuHotkeys, accuMappings)
      | Just hotkey <- chooseHotkey accuHotkeys (getLabel option) =
          (hotkey : accuHotkeys, accuMappings <> [(hotkey, option)])
      | otherwise =
          accu

    chooseHotkey :: [Char] -> [Char] -> Maybe Char
    chooseHotkey used label =
      find (`notElem` used) $
        toLower
          <$> filter isUpper label
            <> filter isLower label
            <> label
            <> ['a' .. 'z']
            <> ['0' .. '9']

    getLabel :: MenuEntry a -> String
    getLabel (Option label _) = into label
    getLabel (SubMenu (Menu label _)) = into label

    wholePrompt :: [String]
    wholePrompt =
      maybe id ((:) . into) prompt $
        into name
          : (if top then ".: Leave Menu" else ".: Back")
          : ((promptLabel <$> mappings) <> ["> "])

    promptLabel :: (Char, MenuEntry a) -> [Char]
    promptLabel (c, option) = toUpper c : (": " <> getLabel option)

confirm :: Character :<: m => Text -> Wizard m Bool
confirm prompt =
  menu Nothing $ Menu prompt [Option "Yes" True, Option "No" False]

-- From Hledger.Cli.Commands.Add
withCompletion
  :: WithSettings :<: m => CompletionFunc IO -> Wizard m a -> Wizard m a
withCompletion c = withSettings (setComplete c defaultSettings)

getLineWithDefaultAndSuggestions
  :: (WithSettings :<: m, LinePrewritten :<: m)
  => PromptString
  -> Maybe String
  -> [String]
  -> Wizard m String
getLineWithDefaultAndSuggestions prompt startInput completions =
  retry . withCompletion completeFunc $
    linePrewritten
      (prompt <> "\n> ")
      prewritten
      ""
 where
  completeFunc (before, _) = pure ("", simpleCompletion <$> match completions)
   where
    match = filter (isInfixOf $ reverse before)
  prewritten = fromMaybe "" startInput

runHaskeline :: MonadIO m => Wizard Haskeline a -> m (Maybe a)
runHaskeline = liftIO . runInputT defaultSettings . run

runClearingHaskeline :: MonadIO m => Wizard Haskeline a -> m (Maybe a)
runClearingHaskeline = liftIO . runInputT defaultSettings . runClearing . run
