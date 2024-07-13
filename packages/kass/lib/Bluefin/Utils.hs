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

inContext' :: e2 :> e1 => Eff (e2 :& e1) r -> Eff e1 r
inContext' = Internal.inContext . Internal.weakenEff (com (# #))

com :: (# #) -> (a :& b) `Internal.In` (b :& a)
com _ = Internal.cmp (Internal.bimap (Internal.sndI (# #)) (Internal.fstI (# #))) (Internal.merge (# #))
