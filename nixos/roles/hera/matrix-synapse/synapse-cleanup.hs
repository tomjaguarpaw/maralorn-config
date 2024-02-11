{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-type-defaults -Wno-orphans #-}

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, decode)
import Data.String.Interpolate (i)
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple as PSQL (
  Connection,
  Only (Only, fromOnly),
  connectPostgreSQL,
  query,
  query_,
 )
import Network.HTTP (
  Header (Header),
  HeaderName (HdrAuthorization),
  RequestMethod (DELETE),
  Request_String,
  Response (rspBody, rspReason),
  getRequest,
  insertHeaders,
  postRequest,
  postRequestWithBody,
  rqMethod,
  rspCode,
  simpleHTTP,
 )
import Relude
import Say (say, sayErr)
import Shh (ExecReference (Absolute), load, (|>))
import System.IO (BufferMode (LineBuffering))

-- Executables used.
load Absolute ["synapse_compress_state", "cat", "psql", "rm"]

newtype PurgeResult = PurgeResult {purge_id :: Text} deriving (Generic, FromJSON)
newtype Status = Status {status :: Text} deriving (Generic, FromJSON)

apiUrl = [i|http://localhost:8008/_synapse/admin/v1|] :: Text
daysOld = 30
lastMessages = 500
minUsersToPurgeRoom = 5
filename = "/var/lib/matrix-synapse/tmp-storage-compression.sql"
contentType = "application/json"

giveToken :: Text -> Request_String -> Request_String
giveToken token = insertHeaders [Header HdrAuthorization [i|Bearer #{token}|]]

getToken :: PSQL.Connection -> IO Text
getToken conn = extractFromList <$> query_ conn queryString
 where
  extractFromList = fromMaybe (error "No admin token in database") . viaNonEmpty head . fmap fromOnly
  queryString = "SELECT token FROM access_tokens JOIN users ON user_id=name WHERE admin=1 ORDER BY id DESC LIMIT 1"

waitForPurge :: Text -> Text -> IO ()
waitForPurge token purgeId = do
  result <- go 1
  say [i|Purge result: #{result}|]
 where
  handeResponse timeout =
    either
      (\e -> pure [i|purge failed with error: #{e}|])
      ( \resp -> do
          let res = maybe [i|couldn‘t parse purge response #{rspBody resp}|] status (decode . encodeUtf8 . rspBody $ resp)
          if res == "active" then go (min (timeout * 2) 60) else pure res
      )
  go timeout = do
    say [i|purge #{purgeId} is going on waiting #{timeout} seconds|]
    threadDelay (timeout * 1000000)
    handeResponse timeout =<< (simpleHTTP . giveToken token . getRequest) [i|#{apiUrl}/purge_history_status/#{purgeId}|]

queryLastKeptEvent :: PSQL.Connection -> Text -> IO (Maybe (Text, UTCTime))
queryLastKeptEvent conn roomId =
  let process = fmap (second (posixSecondsToUTCTime . (/ 1000) . realToFrac)) . viaNonEmpty head . mapMaybe sequence
      queryString = "SELECT event_id, received_ts from events WHERE type='m.room.message' AND room_id =? ORDER BY received_ts DESC LIMIT 1 offset ?"
   in process <$> query conn queryString (roomId, lastMessages - 1)

purgeUpToEvent :: Text -> Text -> UTCTime -> (Text, UTCTime) -> IO ()
purgeUpToEvent token roomId upToTime (eventName, eventTime) =
  handleResponse =<< simpleHTTP . giveToken token . postRequestWithBody url contentType =<< getBody
 where
  upToTimeStamp = floor . (* 1000) . utcTimeToPOSIXSeconds $ upToTime
  timeOutBody = [i|{"delete_local_events":"true", "purge_up_to_ts":#{upToTimeStamp}}|]
  url = [i|#{apiUrl}/purge_history/#{roomId}|]
  getBody =
    if eventTime < upToTime
      then do
        say [i|Deleting up to #{eventName} in #{roomId}.|]
        pure [i|{"delete_local_events":"true", "purge_up_to_event_id":"#{eventName}"}|]
      else do
        say [i|Deleting up to last #{daysOld} days in #{roomId}.|]
        pure timeOutBody
  handleResponse =
    either
      (\e -> sayErr [i|Could not get purge status in #{roomId}. Error: #{e}|])
      ( \resp ->
          maybe
            (sayErr [i|Could not parse purge result: #{rspBody resp}|])
            (\(purge_id -> purgeResult) -> say [i|Purging with id #{purgeResult} for room #{roomId}.|] >> waitForPurge token purgeResult)
            (decode . encodeUtf8 . rspBody $ resp)
      )

purgeRoom :: Text -> Text -> IO ()
purgeRoom token roomID = do
  say [i|Deleting #{roomID}...|]
  handleResponse =<< (simpleHTTP . giveToken token . \x -> x{rqMethod = DELETE}) (postRequestWithBody url contentType "{}")
 where
  url = [i|#{apiUrl}/rooms/#{roomID}|]
  handleResponse = either printErr (\x -> say [i|#{rspCode x}: #{rspReason x}\n#{rspBody x}|])
  printErr e = sayErr [i|Could not purge room #{roomID}. Error #{e}|]

processRoom :: Text -> PSQL.Connection -> UTCTime -> Text -> IO ()
processRoom token conn upToTime roomId = do
  whenJustM (queryLastKeptEvent conn roomId) (purgeUpToEvent token roomId upToTime)
  say [i|Compressing state in room #{roomId} ...|]
  synapse'_compress'_state "-o" filename "-p" "host=/run/postgresql user=matrix-synapse dbname=matrix-synapse" "-r" (toString roomId)
  cat filename |> psql "matrix-synapse"
  rm filename

locallyUnjoinedRoomsQuery = "SELECT r.room_id FROM rooms AS r LEFT JOIN (SELECT room_id FROM local_current_membership WHERE membership = 'join' GROUP BY room_id) AS l ON l.room_id = r.room_id WHERE l.room_id IS NULL"
largeRoomsQuery = "SELECT q.room_id FROM (select count(*) as numberofusers, room_id FROM current_state_events WHERE type ='m.room.member' AND membership = 'join' GROUP BY room_id) AS q LEFT JOIN room_aliases a ON q.room_id=a.room_id WHERE q.numberofusers > ? ORDER BY numberofusers desc"

main :: IO ()
main = do
  _ <- missingExecutables
  hSetBuffering stdout LineBuffering

  upToTime <- addUTCTime ((-1) * realToFrac daysOld * nominalDay) <$> getCurrentTime
  let upToTimeStamp = floor . (* 1000) . utcTimeToPOSIXSeconds $ upToTime

  conn <- connectPostgreSQL "dbname='matrix-synapse'"
  token <- getToken conn

  -- Get rooms without locally member and purge them
  say "Purging obsolete rooms ..."
  obsoleteRoomIds <- fromOnly @Text <<$>> query conn locallyUnjoinedRoomsQuery ()
  mapM_ (purgeRoom token) obsoleteRoomIds

  -- Get large rooms and then
  -- 1. only keep max(last 30 days, 500 events) of history
  -- 2. compress state events
  say "Compressing large rooms ..."
  largeRoomIds <- fromOnly @Text <<$>> query conn largeRoomsQuery (Only minUsersToPurgeRoom)
  mapM_ (processRoom token conn upToTime) largeRoomIds

  say "Pruning remote media ..."
  _ <- simpleHTTP . giveToken token . postRequest $ [i|#{apiUrl}/purge_media_cache/?before_ts=#{upToTimeStamp}|]
  say "Finished"
