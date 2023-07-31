module Maralorn.Prelude ((%), (%>), (%>>), (<<&>>), module Relude) where

import Relude

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
