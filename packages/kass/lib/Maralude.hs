{-# LANGUAGE UnboxedTuples #-}

module Maralude
  ( module Witch
  , module Relude
  , module Optics
  , module Bluefin.Eff
  , module Bluefin.IO
  , module Bluefin.State
  , module Bluefin.Compound
  , module Bluefin.Exception
  , module Bluefin.EarlyReturn
  , module Data.String.Interpolate
  , module System.Exit
  , module Say
  , List
  , assoc1Eff
  , inContext
  , inContext'
  , catMaybes
  , mapMaybe
  , morph
  , isomorph
  , worded
  , lined
  , MonadFix
  , readFile
  )
where

import Bluefin.Compound
import Bluefin.EarlyReturn
import Bluefin.Eff
import Bluefin.Exception hiding (Exception)
import Bluefin.IO
import Bluefin.Internal (In, assoc1Eff, cmp, fstI, inContext, sndI, weakenEff)
import Bluefin.Internal qualified as BF
import Bluefin.State
import Control.Monad.Fix (MonadFix)
import Data.String.Interpolate
import GHC.List (List)
import Optics
import Relude hiding
  ( Handle
  , State
  , catMaybes
  , evalState
  , execState
  , get
  , mapMaybe
  , modify
  , put
  , readFile
  , return
  , runState
  , uncons
  )
import Say
import System.Exit (ExitCode (..))
import Witch
import Witherable (catMaybes, mapMaybe)

readFile :: MonadIO m => FilePath -> m Text
readFile path = into . decodeUtf8 @LText <$> readFileLBS path

worded :: Iso' Text [Text]
worded = iso words unwords

lined :: Iso' Text [Text]
lined = iso lines unlines

inContext' :: e2 :> e1 => Eff (e2 :& e1) r -> Eff e1 r
inContext' = inContext . weakenEff (com (# #))

isomorph :: forall t s. (From s t, From t s) => Iso' s t
isomorph = iso from from

morph :: forall t s. From s t => Getter s t
morph = to from

com :: (# #) -> (a :& b) `In` (b :& a)
com _ = cmp (BF.bimap (sndI (# #)) (fstI (# #))) (BF.merge (# #))
