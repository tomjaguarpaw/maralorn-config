{- This file was automatically generated and should not be edited. -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

module NixpkgsBot.GraphQL.API where

import Data.GraphQL
import Data.GraphQL.Bootstrap

import NixpkgsBot.GraphQL.Scalars

{-----------------------------------------------------------------------------
* PullRequest

-- result :: Object PullRequestSchema; throws a GraphQL exception on errors
result <- runQuery PullRequestQuery
  { _number = ...
  }

-- result :: GraphQLResult (Object PullRequestSchema)
result <- runQuerySafe PullRequestQuery
  { _number = ...
  }
-----------------------------------------------------------------------------}

data PullRequestQuery = PullRequestQuery
  { _number :: Int
  }
  deriving (Show)

type PullRequestSchema =
  [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        title: Text,
        author: Maybe {
          login: Text,
        },
        mergedAt: Maybe DateTime,
        mergedBy: Maybe {
          login: Text,
        },
        mergeCommit: Maybe {
          oid: GitObjectID,
        },
        baseRefName: Text,
      },
    },
  }
|]

instance GraphQLQuery PullRequestQuery where
  type ResultSchema PullRequestQuery = PullRequestSchema

  getQueryName _ = "PullRequest"

  getQueryText _ =
    [query|
    query PullRequest($number: Int!) {
      repository(owner: "NixOS", name: "nixpkgs") {
        pullRequest(number: $number) {
          title
          author {
            login
          }
          mergedAt
          mergedBy {
            login
          }
          mergeCommit {
            oid
          }
          baseRefName
        }
      }
    }
  |]

  getArgs query =
    object
      [ "number" .= _number (query :: PullRequestQuery)
      ]
