module Maralude (
  Eq,
  Show,
  Maybe,
  Text,
  Bool (False, True),
  IO,
  String,
  Identity,
  State,
  MonadIO,
  Stream,
  liftIO,
  to,
  view,
  fmap,
  Functor,
  Monad,
  Applicative,
  (>>),
  (>>=),
  (=<<),
  (.),
  (==),
  _Just,
  _Nothing,
  _Right,
  _Left,
  maybe,
  (&),
  (^?),
  (^.),
  (^..),
  (<),
  (<=),
  (>),
  (>=),
  (<>),
  ($),
  (<$>),
  id,
  Contravariant,
  Iso',
  Prism',
  in',
  to',
  ix,
  hasn't,
  has,
  folded,
  from,
  into,
  getArgs,
  putTextLn,
  pure,
  worded,
  lined,
  flip,
  (%),
  i,
  mapM_,
  mapM,
  compare,
  sequence_,
  each,
  (.~),
  (||),
  (&&),
  (%~),
  _1,
  _2,
  _3,
  otherwise,
  on,
  only,
)
where

import Control.Applicative (Applicative, pure)
import Control.Lens (Contravariant, Iso', LensLike', Prism', each, folded, from, has, hasn't, iso, ix, only, preview, prism', to, view, (%~), (.~), (^.), (^..), (^?), _1, _2, _3, _Just, _Left, _Nothing, _Right)
import Control.Monad (Monad, (=<<), (>>), (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (State)
import Data.Bool (Bool (False, True), otherwise, (&&), (||))
import Data.Either (Either)
import Data.Eq (Eq, (==))
import Data.Function (flip, id, on, ($), (&), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe, maybe)
import Data.Ord (compare, (<), (<=), (>), (>=))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (mapM)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Internal.Data.Stream.StreamD.Eliminate (mapM_)
import System.Environment qualified as Env
import System.IO (IO)
import Text.Show (Show)
import Witch qualified

worded :: Iso' Text [Text]
worded = iso Text.words Text.unwords

lined :: Iso' Text [Text]
lined = iso Text.lines Text.unlines

putTextLn :: (MonadIO m) => Text -> m ()
putTextLn = liftIO . Text.putStrLn
{-# SPECIALIZE putTextLn :: Text -> IO () #-}
{-# INLINE putTextLn #-}

getArgs :: (MonadIO m) => m [Text]
getArgs = fmap (view to') <$> liftIO Env.getArgs

into :: forall target source f. (Witch.From target source, Contravariant f) => LensLike' f target source
into = to (Witch.from)

to' :: forall a b. (Witch.From a b, Witch.From b a) => Iso' a b
to' = iso Witch.into Witch.into

hush :: Either b a -> Maybe a
hush = preview folded

in' :: forall source target. (Witch.TryFrom source target, Witch.From target source) => Prism' source target
in' = prism' Witch.into (hush . Witch.tryInto)

sequence_ :: (Monad m) => Stream m (m a) -> m ()
sequence_ = Stream.sequence % Stream.fold Fold.drain

(%) :: (a -> b) -> (b -> c) -> (a -> c)
(%) = flip (.)
