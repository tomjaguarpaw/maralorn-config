module StatusScript.Modules.Hyprland (hyprlandWorkspaces) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode')
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
import System.Environment (getEnv)

Shh.load Shh.Absolute ["hyprctl"]

missingExecutables :: IO [FilePath]
hyprlandWorkspaces
  :: MonadHeadlessApp t m
  => Env
  -> m (Dynamic t (Seq (HyprlandWorkspace HyprlandWindow)))
hyprlandWorkspaces env = do
  reportMissing missingExecutables
  socket <- liftIO $ Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
  dir <- liftIO $ getEnv "XDG_RUNTIME_DIR"
  hyprL <- liftIO $ getEnv "HYPRLAND_INSTANCE_SIGNATURE"
  let socket_name = [i|#{dir}/hypr/#{hyprL}/.socket2.sock|]
  liftIO $ Network.connect socket (Network.SockAddrUnix socket_name)
  (event, trigger) <- newTriggerEvent
  liftIO $ env.fork [i|Listening on socket #{socket_name}|] $ forever do
    _ <- Network.recv socket 20
    trigger =<< mkInfo
  startInfo <- mkInfo
  holdDyn startInfo event
 where
  mkInfo :: MonadIO m => m (Seq (HyprlandWorkspace HyprlandWindow))
  mkInfo = liftIO do
    active_bs <- hyprctl "activewindow" "-j" |> captureTrim
    all_windows_bs <- hyprctl "clients" "-j" |> captureTrim
    active <- either (\s -> print s >> pure Nothing) pure $ eitherDecode' active_bs
    all_windows <- either (\s -> print s >> pure mempty) pure $ eitherDecode' all_windows_bs
    pure $ calculateLayout active all_windows

calculateLayout :: (Maybe HyprctlClient) -> Seq HyprctlClient -> Seq (HyprlandWorkspace HyprlandWindow)
calculateLayout active all_windows = Seq.fromList $ fmap (mkWindow active) <$> IntMap.elems workspaceMap
 where
  workspaceMap = foldl' (flip addToWorkspaces) mempty all_windows

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
    (Just . addToWorkspace client . fromMaybe (MkHyprlandWorkspace mempty mempty))
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
  , workspace :: HyprctlClientWorkspace
  , initialClass :: Text
  , title :: Text
  , floating :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype HyprctlClientWorkspace = MkHyprctlClientWorkspace
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
  { floating :: Seq a
  , stacks :: Seq (NESeq a)
  }
  deriving stock (Generic, Functor)
  deriving anyclass (ToJSON)
