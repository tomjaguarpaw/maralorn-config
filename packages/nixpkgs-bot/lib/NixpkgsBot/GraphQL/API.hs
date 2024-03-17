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
-- MergingPullRequest

-- result :: Object MergingPullRequestSchema; throws a GraphQL exception on errors
result <- runQuery MergingPullRequestQuery
  { _commit = ...
  , _m_owner = ...
  , _m_name = ...
  }

-- result :: GraphQLResult (Object MergingPullRequestSchema)
result <- runQuerySafe MergingPullRequestQuery
  { _commit = ...
  , _m_owner = ...
  , _m_name = ...
  }
-----------------------------------------------------------------------------}

data MergingPullRequestQuery = MergingPullRequestQuery
  { _commit :: GitObjectID
  , _m_owner :: Text
  , _m_name :: Text
  }
  deriving (Show)

type MergingPullRequestSchema =
  [schema|
  {
    repository: Maybe {
      object: Maybe {
        [__fragment]: Try (
          {
            associatedPullRequests: Maybe {
              nodes: Maybe List Maybe {
                number: Int,
                title: Text,
                mergeCommit: Maybe {
                  oid: GitObjectID,
                },
                author: Maybe {
                  login: Text,
                },
                merged: Bool,
                baseRefName: Text,
              },
            },
          }
        ),
      },
    },
    rateLimit: Maybe {
      remaining: Int,
      resetAt: DateTime,
    },
  }
|]

instance GraphQLQuery MergingPullRequestQuery where
  type ResultSchema MergingPullRequestQuery = MergingPullRequestSchema

  getQueryName _ = "MergingPullRequest"

  getQueryText _ =
    [query|
    query MergingPullRequest($commit: GitObjectID!, $m_owner: String!, $m_name: String!) {
      repository(owner: $m_owner, name: $m_name) {
        object(oid: $commit) {
          ... on Commit {
            associatedPullRequests(first: 10) {
              nodes {
                number
                title
                mergeCommit {
                  oid
                }
                author {
                  login
                }
                merged
                baseRefName
              }
            }
          }
        }
      }
      rateLimit {
        remaining
        resetAt
      }
    }
  |]

  getArgs query =
    object
      [ "commit" .= _commit (query :: MergingPullRequestQuery)
      , "m_owner" .= _m_owner (query :: MergingPullRequestQuery)
      , "m_name" .= _m_name (query :: MergingPullRequestQuery)
      ]

{-----------------------------------------------------------------------------
-- PullRequest

-- result :: Object PullRequestSchema; throws a GraphQL exception on errors
result <- runQuery PullRequestQuery
  { _number = ...
  , _owner = ...
  , _name = ...
  }

-- result :: GraphQLResult (Object PullRequestSchema)
result <- runQuerySafe PullRequestQuery
  { _number = ...
  , _owner = ...
  , _name = ...
  }
-----------------------------------------------------------------------------}

data PullRequestQuery = PullRequestQuery
  { _number :: Int
  , _owner :: Text
  , _name :: Text
  }
  deriving (Show)

type PullRequestSchema =
  [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        number: Int,
        title: Text,
        mergeCommit: Maybe {
          oid: GitObjectID,
        },
        author: Maybe {
          login: Text,
        },
        merged: Bool,
        baseRefName: Text,
      },
    },
    rateLimit: Maybe {
      remaining: Int,
      resetAt: DateTime,
    },
  }
|]

instance GraphQLQuery PullRequestQuery where
  type ResultSchema PullRequestQuery = PullRequestSchema

  getQueryName _ = "PullRequest"

  getQueryText _ =
    [query|
    query PullRequest($number: Int!, $owner: String!, $name: String!) {
      repository(owner: $owner, name: $name) {
        pullRequest(number: $number) {
          number
          title
          mergeCommit {
            oid
          }
          author {
            login
          }
          merged
          baseRefName
        }
      }
      rateLimit {
        remaining
        resetAt
      }
    }
  |]

  getArgs query =
    object
      [ "number" .= _number (query :: PullRequestQuery)
      , "owner" .= _owner (query :: PullRequestQuery)
      , "name" .= _name (query :: PullRequestQuery)
      ]
