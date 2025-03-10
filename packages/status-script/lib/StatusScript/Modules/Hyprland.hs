module StatusScript.Modules.Hyprland (hyprlandWorkspaces) where

import Control.Monad.Extra (untilJustM)
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
import Relude hiding (catMaybes)
import Shh (captureTrim, (|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.Env
import StatusScript.ReflexUtil
import System.Environment (getEnv)
import Witherable (catMaybes)

Shh.load Shh.Absolute ["hyprctl"]

watchHyprland :: IO () -> IO Void
watchHyprland cb = forever $ retryIndefinite 2 watch
 where
  watch = do
    socket <- Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
    loadSystemdEnv "HYPRLAND_INSTANCE_SIGNATURE"
    dir <- getEnv "XDG_RUNTIME_DIR"
    hyprL <- getEnv "HYPRLAND_INSTANCE_SIGNATURE"
    let socket_name = [i|#{dir}/hypr/#{hyprL}/.socket2.sock|]
    Network.connect socket (Network.SockAddrUnix socket_name)
    untilJustM $
      Network.recv socket 1028 >>= \case
        x | ByteString.null x -> pure $ Just ()
        _ -> cb >> pure Nothing

missingExecutables :: IO [FilePath]
hyprlandWorkspaces
  :: MonadHeadlessApp t m
  => Env
  -> m (Dynamic t (Seq (HyprlandWorkspace HyprlandWindow)))
hyprlandWorkspaces env = do
  reportMissing missingExecutables
  (event, trigger) <- newTriggerEvent
  ev <- catMaybes <$> performEventThreaded env event \_ -> retryTimeout 5 10 mkInfo
  liftIO $ env.fork [i|Listening on Hyprland socket|] $ do
    trigger ()
    absurd <$> watchHyprland (trigger ())
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
        (\s -> IntMap.singleton s.id $ MkHyprlandWorkspace True s.monitorID mempty mempty)

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
    (Just . addToWorkspace client . fromMaybe (MkHyprlandWorkspace False client.monitor mempty mempty))
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
  , monitor :: Int
  , workspace :: HyprctlClientWorkspace
  , initialClass :: Text
  , title :: Text
  , floating :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype HyprctlClientWorkspace = MkHyprctlClientWorkspace
  { id :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data HyprctlWorkspace = MkHyprctlWorkspace
  { id :: Int
  , monitorID :: Int
  }
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
  , monitor :: Int
  , floating :: Seq a
  , stacks :: Seq (NESeq a)
  }
  deriving stock (Generic, Functor)
  deriving anyclass (ToJSON)
