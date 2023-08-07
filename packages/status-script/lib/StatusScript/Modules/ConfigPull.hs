module StatusScript.Modules.ConfigPull (pullNeeded) where

import Data.ByteString.Lazy.Char8 qualified as LBSC
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.Environment qualified as Env
import System.FilePath ((</>))

Shh.load Shh.Absolute ["git"]
missingExecutables :: IO [FilePath]
pullNeeded :: R.MonadHeadlessApp t m => R.Dynamic t Mode -> m (R.Event t [Warning])
pullNeeded mode = do
  CommandUtil.reportMissing missingExecutables
  tick <- ReflexUtil.tickEvent (5 * 60)
  home <- liftIO $ Env.getEnv "HOME"
  ReflexUtil.performEventThreaded (ReflexUtil.taggedAndUpdated mode tick) \case
    Klausur -> pure []
    _ -> do
      behind <-
        CommandUtil.tryCmd (git "--no-optional-locks" "-C" (home </> "git" </> "config") "log" "--oneline" "origin/main" "^main")
          <&> LBSC.lines % length
      pure
        [ MkWarning
          { description =
              Just
                [i|Config #{behind} commits behind.|]
          , group = "warning"
          , subgroup = Nothing
          }
        | 0 < behind
        ]
