module StatusScript.Modules.Mako (notifications) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Schema (get, schema)
import Data.Aeson.Schema qualified as Schema
import Maralorn.Prelude hiding (get)
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings

type MakoList =
  [schema|{
   data: List List Value
  }|]

Shh.load Shh.Absolute ["makoctl"]

missingExecutables :: IO [FilePath]
notifications :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Warning])
notifications = \env -> do
  CommandUtil.reportMissing missingExecutables
  tick <- ReflexUtil.tickEvent 5
  ReflexUtil.performEventThreaded env tick \_ ->
    CommandUtil.tryCmd (makoctl "list")
      <&> Aeson.decode @(Schema.Object MakoList)
        %> [get|.data|]
        %> join
        % fromMaybe []
        %> const
          MkWarning
            { group = "notification"
            , subgroup = Nothing
            , description = Nothing
            }
