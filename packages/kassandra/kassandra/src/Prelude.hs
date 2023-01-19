{-# OPTIONS_GHC -Wno-deprecations #-}

module Prelude (
  module Relude,
  module Optics,
  module Optics.TH,
  module Data.Text.Optics,
  partitionEithersNE,
  Task,
  i,
  (<$?>),
  (<&?>),
  Aeson.ToJSON,
  Aeson.FromJSON,
  getZonedTime,
  addUTCTime,
  utcToZonedTime,
  zonedTimeZone,
  zonedTimeToUTC,
  firstJust,
  IOException,
  UTCTime,
  catch,
  concurrently_,
  forConcurrently_,
  race_,
  makeLabels,
  Status,
  MonadFix,
  ZonedTime,
  NominalDiffTime,
  toJSON,
  fromJSON,
  UUID,
  HasField',
  field',
  HasAny,
  the,
  HasType,
  typed,
  AsType,
  _Typed,
  AsConstructor',
  _Ctor',
  formatTime,
  zonedTimeToLocalTime,
  LocalTime,
  defaultTimeLocale,
  parseTimeM,
  NESeq,
  nonEmptySeq,
  pattern IsEmpty,
  pattern IsNonEmpty,
  pattern (:<||),
  pattern (:||>),
  (|>),
  (<|),
  toSeq,
  mapMaybe,
  filter,
  partitionEithersNESeq,
) where

import Control.Concurrent.Async (
  concurrently_,
  forConcurrently_,
  race_,
 )
import Control.Exception (
  IOException,
  catch,
 )
import Control.Monad.Fix (MonadFix)
import Data.Aeson (
  fromJSON,
  toJSON,
 )
import Data.Aeson qualified as Aeson
import Data.Generics.Product.Any (HasAny (the))
import Data.Generics.Product.Fields (HasField' (field'))
import Data.Generics.Product.Typed (HasType (typed))
import Data.Generics.Sum.Constructors (AsConstructor' (_Ctor'))
import Data.Generics.Sum.Typed (AsType (_Typed))
import Data.List.Extra (firstJust)
import Data.Sequence ((<|), (|>))
import Data.Sequence.NonEmpty hiding (filter, (<|), (|>))
import Data.String.Interpolate (i)
import Data.Text.Optics hiding (text)
import Data.These (These (..), partitionEithersNE)
import Data.Time (
  UTCTime,
  addUTCTime,
  defaultTimeLocale,
  formatTime,
  getZonedTime,
  parseTimeM,
  utcToZonedTime,
  zonedTimeToUTC,
  zonedTimeZone,
 )
import Data.Time.Clock (NominalDiffTime)
import Data.Time.LocalTime (
  LocalTime,
  ZonedTime,
  zonedTimeToLocalTime,
 )
import Data.UUID (UUID)
import Data.Witherable (filter, mapMaybe, (<$?>), (<&?>))
import Language.Haskell.TH.Syntax (
  Dec,
  Name,
  Q,
 )
import Optics hiding ((<|), (|>))
import Optics.TH
import Relude hiding (filter, mapMaybe, uncons)
import Relude.Extra.Foldable1
import Taskwarrior.Status (Status)
import Taskwarrior.Task (Task)

instance One (NESeq a) where
  type OneItem (NESeq a) = a
  one = singleton

instance Foldable1 NESeq where
  foldMap1 f = foldMapWithIndex (const f)

-- (lensField .~ noPrefixNamer $ fieldLabelsRules) == noPrefixFieldLabels but only in optics-th 0.2
makeLabels :: Name -> Q [Dec]
makeLabels = makeFieldLabelsWith noPrefixFieldLabels

partitionEithersNESeq :: NESeq (Either a b) -> These (NESeq a) (NESeq b)
partitionEithersNESeq = fold1 . fmap (either (This . one) (That . one))
