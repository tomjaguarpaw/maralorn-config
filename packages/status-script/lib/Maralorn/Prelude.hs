module Maralorn.Prelude (say, sayErr, hush, (%), (%>), (%>>), (<<&>>), module Relude, i, s, t) where

import Data.String.Interpolate (i)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Relude
import Say (say, sayErr)

asString :: (String -> Q Exp) -> String -> Q Exp
asString qq str =
  [|$(qq str) :: String|]

asText :: (String -> Q Exp) -> String -> Q Exp
asText qq str =
  [|$(qq str) :: Text|]

s :: QuasiQuoter
s =
  QuasiQuoter
    { quoteExp = asString $ quoteExp i
    , quotePat = quotePat i
    , quoteType = quoteType i
    , quoteDec = quoteDec i
    }

t :: QuasiQuoter
t =
  QuasiQuoter
    { quoteExp = asText $ quoteExp i
    , quotePat = quotePat i
    , quoteType = quoteType i
    , quoteDec = quoteDec i
    }

hush :: Either a1 a2 -> Maybe a2
hush = \case
  Left _ -> Nothing
  Right x -> Just x

infixl 9 %

(%) :: (a -> b) -> (b -> c) -> a -> c
f % g = g . f

infixl 9 %>

(%>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f %> g = fmap g . f

infixl 9 %>>

(%>>) :: (Functor g, Functor f) => (a -> f (g b)) -> (b -> c) -> a -> f (g c)
f %>> g = fmap (fmap g) . f

infixl 1 <<&>>

(<<&>>) :: (Functor g, Functor f) => f (g a) -> (a -> b) -> f (g b)
x <<&>> g = fmap (fmap g) x
