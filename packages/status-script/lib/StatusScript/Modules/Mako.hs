module StatusScript.Modules.Mako (notifications) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Schema (get, schema)
import Data.Aeson.Schema qualified as Schema
import Maralorn.Prelude hiding (get)
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import Shh (captureTrim, (|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.Mode
import StatusScript.ReflexUtil (taggedAndUpdated)
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings
import System.Which (which)

type MakoList =
  [schema|{
   data: List List Value
  }|]

Shh.load Shh.Absolute ["makoctl"]

missingExecutables :: IO [FilePath]
notifications :: MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
notifications = \env mode -> do
  CommandUtil.reportMissing missingExecutables
  tick <- ReflexUtil.tickEvent 5
  trigger_ev <-
    liftIO (which "makoctl") <&> \case
      Nothing -> never
      Just _ -> taggedAndUpdated mode tick
  ev <- ReflexUtil.performEventThreaded env trigger_ev \_ ->
    retryWithBackoff (makoctl "list" |> captureTrim)
      <&> Aeson.decode @(Schema.Object MakoList)
      %> [get|.data|]
      %> join
      % fromMaybe []
      %> const
        MkWarning
          { group = toEnum 985604
          , heading = "Benachrichtigungen"
          , subgroup = Nothing
          , barDisplay = Count
          , description = []
          }
  warning_dyn <- holdDyn [] ev
  pure $
    zipDynWith
      ( \case
          DND -> const []
          _ -> id
      )
      mode
      warning_dyn
