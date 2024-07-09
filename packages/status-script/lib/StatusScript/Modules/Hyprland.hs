module StatusScript.Modules.Hyprland (hyprlandWorkspaces) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (catchAny)
import Data.Aeson (FromJSON, ToJSON, decode', eitherDecode')
import Data.ByteString qualified as ByteString
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.String.Interpolate (i)
import Network.Socket qualified as Network
import Network.Socket.ByteString qualified as Network
import Optics (has, only, over, view, (%), (^.), _1, _2)
import Reflex (Dynamic, TriggerEvent (..), holdDyn)
import Reflex.Host.Headless (MonadHeadlessApp)
import Relude
import Shh (captureTrim, (|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.Env
import StatusScript.ReflexUtil
import System.Environment (getEnv)

Shh.load Shh.Absolute ["hyprctl"]

watchHyprland :: IO () -> IO ()
watchHyprland act = forever do
  catchAny watch \err -> do
    print err
    threadDelay 2_000_000
 where
  watch = do
    socket <- Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
    loadSystemdEnv "HYPRLAND_INSTANCE_SIGNATURE"
    dir <- getEnv "XDG_RUNTIME_DIR"
    hyprL <- getEnv "HYPRLAND_INSTANCE_SIGNATURE"
    let socket_name = [i|#{dir}/hypr/#{hyprL}/.socket2.sock|]
    Network.connect socket (Network.SockAddrUnix socket_name)
    forever do
      val <- Network.recv socket 1028
      when (ByteString.null val) $ fail "Hyprland socket returned empty"
      act

missingExecutables :: IO [FilePath]
hyprlandWorkspaces
  :: MonadHeadlessApp t m
  => Env
  -> m (Dynamic t (Seq (HyprlandWorkspace HyprlandWindow)))
hyprlandWorkspaces env = do
  reportMissing missingExecutables
  (event, trigger) <- newTriggerEvent
  ev <-
    performEventThreaded env event \_ -> catchAny mkInfo \err -> do
      print err
      pure mempty
  liftIO $ env.fork [i|Listening on Hyprland socket|] $ do
    threadDelay 10_000
    trigger ()
    watchHyprland $ trigger ()
  holdDyn mempty ev
 where
  mkInfo :: IO (Seq (HyprlandWorkspace HyprlandWindow))
  mkInfo = do
    active_bs <- hyprctl "activewindow" "-j" |> captureTrim
    active_ws_bs <- hyprctl "activeworkspace" "-j" |> captureTrim
    all_windows_bs <- hyprctl "clients" "-j" |> captureTrim
    let active = decode' active_bs
        active_workspace = decode' active_ws_bs
    all_windows <- either (\s -> print s >> pure mempty) pure $ eitherDecode' all_windows_bs
    pure $ calculateLayout active_workspace active all_windows

calculateLayout
  :: (Maybe HyprctlWorkspace) -> (Maybe HyprctlClient) -> Seq HyprctlClient -> Seq (HyprlandWorkspace HyprlandWindow)
calculateLayout active_workspace active all_windows = Seq.fromList $ fmap (mkWindow active) <$> IntMap.elems workspaceMap
 where
  workspaceMap = foldl' (flip addToWorkspaces) initial_ws_map all_windows
  initial_ws_map =
    active_workspace
      & maybe
        mempty
        (\s -> IntMap.singleton s.id $ MkHyprlandWorkspace True mempty mempty)

mkWindow :: Maybe HyprctlClient -> HyprctlClient -> HyprlandWindow
mkWindow active client =
  MkHyprlandWindow
    { active = maybe False (has (#address % only (client.address))) active
    , appId = client.initialClass
    , title = client.title
    }

addToWorkspaces
  :: HyprctlClient -> IntMap (HyprlandWorkspace HyprctlClient) -> IntMap (HyprlandWorkspace HyprctlClient)
addToWorkspaces client =
  IntMap.alter
    (Just . addToWorkspace client . fromMaybe (MkHyprlandWorkspace False mempty mempty))
    client.workspace.id

addToWorkspace :: HyprctlClient -> HyprlandWorkspace HyprctlClient -> HyprlandWorkspace HyprctlClient
addToWorkspace client
  | client.floating = over #floating (Seq.|> client)
  | otherwise = over #stacks (addToStacks client)

addToStacks :: HyprctlClient -> Seq (NESeq HyprctlClient) -> Seq (NESeq HyprctlClient)
addToStacks client stacks =
  before <> case after of
    next_stack :<| rest
      | stackPos next_stack == client ^. #at % _1 ->
          (next_stack & NESeq.sortOn (view (#at % _2)) . (client NESeq.<|)) :<| rest
    rest -> NESeq.singleton client :<| rest
 where
  (before, after) = Seq.breakl (\s -> stackPos s >= client ^. #at % _1) stacks

stackPos :: NESeq HyprctlClient -> Int
stackPos stack = NESeq.head stack ^. #at % _1

-- Input
data HyprctlClient = MkHyprctlClient
  { address :: Text
  , at :: (Int, Int)
  , size :: (Natural, Natural)
  , workspace :: HyprctlWorkspace
  , initialClass :: Text
  , title :: Text
  , floating :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype HyprctlWorkspace = MkHyprctlWorkspace
  {id :: Int}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- Output
data HyprlandWindow = MkHyprlandWindow
  { appId :: Text
  -- ^ e.g "firefox"
  , active :: Bool
  , title :: Text
  -- ^ e.g. "Hoogle -- Mozilla Firefox"
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data HyprlandWorkspace a = MkHyprlandWorkspace
  { active :: Bool
  , floating :: Seq a
  , stacks :: Seq (NESeq a)
  }
  deriving stock (Generic, Functor)
  deriving anyclass (ToJSON)
