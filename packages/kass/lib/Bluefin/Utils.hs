{-# LANGUAGE UnboxedTuples #-}

module Bluefin.Utils where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Internal qualified as Internal
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Relude

async :: e :> es => IOE e -> Eff es a -> Eff es (Async a)
async = \io act -> withEffToIO (\runInIO -> Async.async $ runInIO (const (useImpl act))) io

comEff :: Eff (e1 :& e2) r -> Eff (e2 :& e1) r
comEff = Internal.weakenEff (com (# #))

com :: (# #) -> (a :& b) `Internal.In` (b :& a)
com _ = Internal.cmp (Internal.bimap (Internal.sndI (# #)) (Internal.fstI (# #))) (Internal.merge (# #))
