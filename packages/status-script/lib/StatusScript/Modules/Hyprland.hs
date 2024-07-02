module StatusScript.Modules.Hyprland (hyprlandWorkspaces) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode')
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Optics (has, only, over, view, (%), (^.), _1, _2)
import Reflex (Dynamic, Reflex, constDyn)
import Relude
import Shh (captureTrim, (|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.Env

Shh.load Shh.Absolute ["hyprctl"]

missingExecutables :: IO [FilePath]
hyprlandWorkspaces :: (MonadIO m, Reflex t) => Env -> m (Dynamic t (Seq (HyprlandWorkspace HyprlandWindow)))
hyprlandWorkspaces _env = do
  reportMissing missingExecutables
  active_bs <- liftIO $ hyprctl "activewindow" "-j" |> captureTrim
  all_windows_bs <- liftIO $ hyprctl "clients" "-j" |> captureTrim
  active <- either (\s -> print s >> pure Nothing) pure $ eitherDecode' active_bs
  all_windows <- either (\s -> print s >> pure mempty) pure $ eitherDecode' all_windows_bs
  pure $ constDyn $ calculateLayout active all_windows

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
