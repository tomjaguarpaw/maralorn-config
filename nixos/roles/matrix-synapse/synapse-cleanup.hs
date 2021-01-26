{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-type-defaults -Wno-orphans #-}

import           Control.Concurrent
import           Data.Aeson
import           Data.String.Interpolate
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.PostgreSQL.Simple
import           Network.HTTP
import           Prelude                        ( )
import           Relude
import           Say
import           System.IO
import Shh

load Absolute ["synapse-compress-state", "cat", "psql", "rm", "grep"]

newtype PurgeResult = PurgeResult
  { purge_id :: Text
  }
  deriving (Generic, FromJSON)
newtype Status = Status
  { status :: Text
  }
  deriving (Generic, FromJSON)

apiUrl :: Text
apiUrl = [i|http://localhost:8008/_synapse/admin/v1|]
daysOld = 30
lastMessages = 500
minUsersToPurgeRoom = 5
filename = "/var/lib/matrix-synapse/tmp-storage-compression.sql"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  upToTime <-
    addUTCTime ((-1) * realToFrac daysOld * nominalDay) <$> getCurrentTime
  conn            <- connectPostgreSQL "dbname='matrix-synapse'"
  (token :: Text) <-
    fromMaybe (error "No admin token in database")
    .   viaNonEmpty head
    <$> (fromOnly <<$>> query_
          conn
          "SELECT token FROM access_tokens JOIN users ON user_id=name WHERE admin=1 ORDER BY id DESC LIMIT 1"
        )
  let
    upToTimeStamp = floor . (* 1000) . utcTimeToPOSIXSeconds $ upToTime
    timeOutBody =
      [i|{"delete_local_events":"true", "purge_up_to_ts":#{upToTimeStamp}}|]
    contentType = "application/json"
    setAuth     = insertHeaders [Header HdrAuthorization [i|Bearer #{token}|]]
    wait :: Text -> IO Text
    wait purgeId = go 1
     where
      go timeout = do
        say [i|purge #{purgeId} is going on waiting #{timeout} seconds|]
        threadDelay (timeout * 1000000)
        response <-
          simpleHTTP
          . setAuth
          . getRequest
          $ [i|#{apiUrl}/purge_history_status/#{purgeId}|]
        case response of
          Left  e    -> pure [i|purge failed with error: #{e}|]
          Right resp -> do
            let res =
                  maybe [i|couldn‘t parse purge response #{rspBody resp}|] status (decode . encodeUtf8 . rspBody $ resp)
            if res == "active" then go (min (timeout * 2) 60) else pure res
  say "Pruning remote media ..."
  _ <- simpleHTTP
          . setAuth
          . postRequest
          $ [i|#{apiUrl}/purge_media_cache/?before_ts=#{upToTimeStamp}|]
  roomIds <- fromOnly @Text <<$>> query
    conn
    "SELECT q.room_id FROM (select count(*) as numberofusers, room_id FROM current_state_events WHERE type ='m.room.member' GROUP BY room_id) AS q LEFT JOIN room_aliases a ON q.room_id=a.room_id WHERE q.numberofusers > ? ORDER BY numberofusers desc"
    (Only minUsersToPurgeRoom)
  forM_ roomIds $ \roomId -> do
    synapse_compress_state "-o" filename "-p" "host=/run/postgresql user=matrix-synapse dbname=matrix-synapse" "-r" (toString roomId) |> grep "-v" "DELETE\\|INSERT"
    cat filename |> psql "matrix-synapse"
    rm filename
    eventId <-
      fmap (second (posixSecondsToUTCTime . (/ 1000) . realToFrac))
      .   viaNonEmpty head
      .   mapMaybe sequence
      <$> query
            conn
            "SELECT event_id, received_ts from events WHERE type='m.room.message' AND room_id =? ORDER BY received_ts DESC LIMIT 1 offset ?"
            (roomId, lastMessages - 1)
    let url = [i|#{apiUrl}/purge_history/#{roomId}|]
    whenJust eventId $ \(name :: Text, timestamp) -> do
      response <-
        simpleHTTP
        .   setAuth
        .   postRequestWithBody url contentType
        =<< if timestamp < upToTime
              then
                (do
                  say [i|Deleting up to #{name} in #{roomId}.|]
                  pure
                    [i|{"delete_local_events":"true", "purge_up_to_event_id":"#{name}"}|]
                )
              else do
                say [i|Deleting up to last #{daysOld} days in #{roomId}.|]
                pure timeOutBody
      case response of
        Left e ->
          sayErr [i|Could not get purge status in #{roomId}. Error: #{e}|]
        Right resp -> do
          maybe
            (sayErr [i|Could not parse purge result: #{rspBody resp}|])
            (\purgeResult -> do
              say
                [i|Purging with id #{purge_id purgeResult} for room #{roomId}.|]
              result <- wait (purge_id purgeResult)
              say [i|Purge result: #{result}|]
            )
            (decode . encodeUtf8 . rspBody $ resp)
    -- TODO: run matrix-state-optimizer
