module StatusScript.Modules.Mako (notifications) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Schema (get, schema)
import Data.Aeson.Schema qualified as Schema
import Maralorn.Prelude hiding (get)
import Reflex (Dynamic, holdDyn, zipDynWith)
import Reflex.Host.Headless (MonadHeadlessApp)
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.Mode
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings

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
  ev <- ReflexUtil.performEventThreaded env tick \_ ->
    CommandUtil.tryCmd (makoctl "list")
      <&> Aeson.decode @(Schema.Object MakoList)
      %> [get|.data|]
      %> join
      % fromMaybe []
      %> const
        MkWarning
          { group = toEnum 985604
          , subgroup = Nothing
          , description = Nothing
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
