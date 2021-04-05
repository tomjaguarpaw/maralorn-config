{-# LANGUAGE BlockArguments #-}

module Kassandra.Standalone.State (
  localBackendProvider,
) where

import Control.Concurrent.STM (TQueue, readTQueue)
import Control.Concurrent.STM.TVar (stateTVar)
import Control.Monad.STM (retry)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Network.Simple.TCP as Net
import Say (say, sayErr)
import Taskwarrior.IO (getTasks, saveTasks)

import Kassandra.Api (
  SocketMessage (..),
  SocketRequest (..),
 )
import Kassandra.Backend.Calendar
import Kassandra.Config (LocalBackend, UserConfig (..))
import Kassandra.Debug
import Kassandra.LocalBackend (
  LocalBackendRequest (LocalBackendRequest),
  alive,
  requestQueue,
  responseCallback,
  userConfig,
 )

waitTillFalse :: MonadIO m => TVar Bool -> m ()
waitTillFalse boolTvar = (atomically . whenM (readTVar boolTvar)) retry
foreverWhileTrue :: TVar Bool -> IO a -> IO ()
foreverWhileTrue boolTvar action = (withAsync (forever action) . const . waitTillFalse) boolTvar
lookupTMap :: (Ord k, MonadIO f) => k -> TVar (Map k a) -> f (Maybe a)
lookupTMap key tvarMap = Map.lookup key <$> readTVarIO tvarMap
insertOrAddTMap :: Ord k => k -> e -> TVar (Map k (Seq e)) -> STM Bool
insertOrAddTMap key entry tvarMap =
  stateTVar tvarMap \theMap ->
    second (\x -> Map.insert key x theMap) $
      Map.lookup key theMap & \case
        Just listeners -> (False, listeners |> entry)
        Nothing -> (True, one entry)

type ClientMap = Map LocalBackend (Seq (TVar Bool, SocketMessage -> IO ()))

localBackendProvider :: TQueue LocalBackendRequest -> IO ()
localBackendProvider requestQueue = newTVarIO mempty >>= handleRequests requestQueue

handleRequests :: TQueue LocalBackendRequest -> TVar ClientMap -> IO ()
handleRequests requestQueue mapVar = do
  cache <- newCache
  let go = atomically (readTQueue requestQueue) >>= \req -> withAsync (handleRequest req cache mapVar) $ const go
  withAsync (void (getEvents cache)) (const go)

monitorCallback :: LocalBackend -> TVar ClientMap -> NonEmpty Task -> IO ()
monitorCallback key mapVar tasks = whenJustM (lookupTMap key mapVar) $ mapM_ (($ TaskUpdates tasks) . snd)

handleRequest :: LocalBackendRequest -> Cache -> TVar ClientMap -> IO ()
handleRequest req cache mapVar =
  forConcurrently_
    [ do handleRequestsWhileAlive req cache; removeClientFromMap req mapVar
    , launchOrAttachMonitor req mapVar
    , responseCallback req ConnectionEstablished
    , say "Client registered on backend"
    ]
    id

removeClientFromMap :: MonadIO m => LocalBackendRequest -> TVar ClientMap -> m ()
removeClientFromMap req mapVar = atomically (modifyTVar' mapVar updateMap)
 where
  key = localBackend . userConfig $ req
  filterClients = Seq.filter ((/= alive req) . fst)
  setEntry newEntry clientMap
    | null newEntry = Map.delete key clientMap
    | otherwise = Map.insert key newEntry clientMap
  updateMap clientMap =
    Map.lookup key clientMap & \case
      Just clients -> setEntry (filterClients clients) clientMap
      Nothing -> clientMap

launchOrAttachMonitor :: LocalBackendRequest -> TVar ClientMap -> IO ()
launchOrAttachMonitor LocalBackendRequest{userConfig, alive, responseCallback} mapVar =
  whenM (atomically $ insertOrAddTMap localBackend entry mapVar) $
    do
      whileClientsNotEmpty $ taskMonitor localBackend (monitorCallback localBackend mapVar)
      say "Stopped listening for changes"
 where
  UserConfig{localBackend} = userConfig
  entry = (alive, responseCallback)
  whileClientsNotEmpty action =
    withAsync action . const . atomically . whenJustM (Map.lookup localBackend <$> readTVar mapVar) . const $ retry

-- TODO: Use backend config

handleRequestsWhileAlive :: LocalBackendRequest -> Cache -> IO ()
handleRequestsWhileAlive LocalBackendRequest{userConfig, alive, responseCallback, requestQueue} cache =
  foreverWhileTrue alive $
    atomically (readTQueue requestQueue) >>= \case
      UIConfigRequest -> (responseCallback . UIConfigResponse . uiConfig) userConfig
      AllTasks -> whenNotNullM (getTasks []) (responseCallback . TaskUpdates)
      ChangeTasks tasks -> (saveTasks . toList) tasks
      SetCalendarList uid list -> do
        setList cache uid list
        sendCalendarEvents
      CalenderRequest -> sendCalendarEvents
 where sendCalendarEvents = do
        events <- getEvents cache
        log Debug [i|Sending #{length events} events to client.|]
        responseCallback . CalendarEvents $ events

taskMonitor :: LocalBackend -> (NonEmpty Task -> IO ()) -> IO ()
taskMonitor _ newTasksCallBack = do
  say "Listening for changed or new tasks on 127.0.0.1:6545."
  Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) -> Net.recv socket 4096 >>= unwrapChanges
 where
  unwrapChanges = maybe (sayErr "Unsuccessful connection attempt.") handleChanges
  handleChanges changes =
    either
      (\err -> sayErr [i|Couldn‘t decode #{changes} as Task: #{err}|])
      (newTasksCallBack . one)
      . Aeson.eitherDecodeStrict @Task
      $ changes
