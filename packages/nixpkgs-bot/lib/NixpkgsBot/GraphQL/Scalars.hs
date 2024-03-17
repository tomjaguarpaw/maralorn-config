module NixpkgsBot.GraphQL.Scalars (DateTime, GitObjectID, module Relude) where

import Data.Time.Clock
import Relude

type DateTime = UTCTime

type GitObjectID = Text
