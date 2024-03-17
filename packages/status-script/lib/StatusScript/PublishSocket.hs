module StatusScript.PublishSocket (publish, publishJson, socketsDir) where

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Maralorn.Prelude
import Network.Socket qualified as Network
import Network.Socket.ByteString qualified as Network
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))

mkSendToSocketCallback :: Env -> FilePath -> IO (ByteString -> IO ())
mkSendToSocketCallback = \env socket_name -> do
  server_socket <- Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
  Network.bind server_socket (Network.SockAddrUnix socket_name)
  Network.listen server_socket 5
  client_socket_var <- newEmptyTMVarIO
  last_message_var <- newEmptyTMVarIO
  env.fork [i|Listening on socket #{socket_name}|] $ forever do
    (client_socket, _) <- Network.accept server_socket
    (old_socket_may, message_may) <- atomically $ (,) <$> tryTakeTMVar client_socket_var <*> tryReadTMVar last_message_var
    whenJust old_socket_may Network.close
    no_error <-
      message_may & \case
        Just msg -> send client_socket msg
        Nothing -> pure True
    sayErr [i|New connection on #{socket_name}.|]
    if no_error
      then atomically $ putTMVar client_socket_var client_socket
      else Network.close client_socket
  return \msg -> do
    client_socket <- atomically $ do
      void $ tryTakeTMVar last_message_var -- clear
      putTMVar last_message_var msg
      takeTMVar client_socket_var
    no_error <- send client_socket msg
    if no_error
      then atomically $ putTMVar client_socket_var client_socket
      else Network.close client_socket
 where
  send socket msg =
    Exception.try @Exception.IOException (Network.sendAll socket (msg <> "\n")) <&> isRight

publish
  :: R.MonadHeadlessApp t m
  => Env
  -> Text
  -> R.Event t ByteString
  -> m ()
publish = \env socket_name event -> do
  -- Listen socket
  callback <- liftIO $ mkSendToSocketCallback env [i|#{socketsDir}/#{socket_name}|]
  R.performEvent_ $ event <&> (callback % env.fork [i|Publishing to socket #{socket_name}|] % liftIO)

publishJson
  :: (R.MonadHeadlessApp t m, Aeson.ToJSON a)
  => Env
  -> Text
  -> R.Event t a
  -> m ()
publishJson = \env name -> fmap (Aeson.encode % toStrict) % publish env name

socketsDir :: FilePath
socketsDir = "/run/user/1000/status"
